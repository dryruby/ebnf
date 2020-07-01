# -*- encoding: utf-8 -*-
require 'rdf'
require 'strscan'    unless defined?(StringScanner)

##
# Serialize ruleset back to EBNF
module EBNF
  class Writer
    LINE_LENGTH = 80

    ##
    # Format rules to a String
    #
    # @param  [Array<Rule>] rules
    # @param [:abnf, :ebnf] format (:ebnf)
    # @return [Object]
    def self.string(*rules, format: :ebnf)
      require 'stringio' unless defined?(StringIO)
      buf = StringIO.new
      write(buf, *rules, format: format)
      buf.string
    end

    ##
    # Format rules to $stdout
    #
    # @param  [Array<Rule>] rules
    # @param [:abnf, :ebnf] format (:ebnf)
    # @return [Object]
    def self.print(*rules, format: :ebnf)
      write($stdout, *rules, format: format)
    end

    ##
    # Write formatted rules to an IO like object
    #
    # @param  [Object] out
    # @param  [Array<Rule>] rules
    # @param [:abnf, :ebnf] format (:ebnf)
    # @return [Object]
    def self.write(out, *rules, format: :ebnf)
      Writer.new(rules, out: out, format: format)
    end

    ##
    # Write formatted rules to an IO like object as HTML
    #
    # @param  [Array<Rule>] rules
    # @param [:abnf, :ebnf] format (:ebnf)
    # @return [Object]
    def self.html(*rules, format: :ebnf)
      require 'stringio' unless defined?(StringIO)
      buf = StringIO.new
      Writer.new(rules, out: buf, html: true, format: format)
      buf.string
    end

    ##
    # @param [Array<Rule>] rules
    # @param [Hash{Symbol => Object}] options
    # @param [#write] out ($stdout)
    # @param [:abnf, :ebnf] format (:ebnf)
    # @option options [Symbol] format
    # @option options [Boolean] html (false)
    def initialize(rules, out: $stdout, html: false, format: :ebnf, **options)
      @options = options.dup
      return if rules.empty?

      # Determine max LHS length
      max_id = rules.max_by {|r| r.id.to_s.length}.id.to_s.length
      max_sym = rules.max_by {|r| r.sym.to_s.length}.sym.to_s.length
      lhs_length = max_sym + 3
      lhs_fmt = "%<sym>-#{max_sym}s ::= "
      if max_id > 0
        lhs_fmt = "%<id>-#{max_id+2}s " + lhs_fmt
        lhs_length += max_id + 3
      end
      rhs_length = LINE_LENGTH - lhs_length

      if html
        # Output as formatted HTML
        begin
          require 'haml'
          hout = Haml::Engine.new(HAML_DESC).render(self, rules: rules, format: format) do |rule|
            case format
            when :abnf
              formatted_expr = format_abnf(rule.expr)
              formatted_expr.length > rhs_length ? format_abnf(rule.expr, sep: "\n") : formatted_expr
            when :ebnf
              formatted_expr = format_ebnf(rule.expr)
              formatted_expr.length > rhs_length ? format_ebnf(rule.expr, sep: "\n") : formatted_expr
            end
          end
          out.write hout
          return
        rescue LoadError
          $stderr.puts "Generating HTML requires haml gem to be loaded"
        end
      end

      # Format each rule, considering the available rhs size
      rules.each do |rule|
        buffer = if rule.pass?
          "%-#{lhs_length-2}s" % "@pass"
        else
          lhs_fmt % {id: "[#{rule.id}]", sym: rule.sym}
        end
        if format == :abnf
          formatted_expr = format_abnf(rule.expr)
          if formatted_expr.length > rhs_length
            buffer << format_abnf(rule.expr, sep: ("\n" + " " * lhs_length))
          else
            buffer << formatted_expr
          end
        elsif format == :ebnf
          formatted_expr = format_ebnf(rule.expr)
          if formatted_expr.length > rhs_length
            buffer << format_ebnf(rule.expr, sep: ("\n" + " " * lhs_length))
          else
            buffer << formatted_expr
          end
        end
        out.puts(buffer)
      end
    end

    protected

    ##
    # W3C EBNF Formatters
    ##

    # Format the expression part of a rule
    def format_ebnf(expr, sep: nil, embedded: false)
      return (@options[:html] ? %(<a href="#grammar-production-#{expr}">#{expr}</a>) : expr.to_s) if expr.is_a?(Symbol)
      if expr.is_a?(String)
        if expr.length == 1
          return format_ebnf_char(expr)
        elsif expr =~ /\A#x\h+/
          return (@options[:html] ? %(<code class="grammar-char-escape">#{expr}</code>) : expr)
        elsif expr =~ /"/
          return (@options[:html] ? %('<code class="grammar-literal">#{escape(expr, "'")}</code>') : %('#{escape(expr, "'")}'))
        else
          return (@options[:html] ? %("<code class="grammar-literal">#{escape(expr, '"')}</code>") : %("#{escape(expr, '"')}"))
        end
      end
      parts = {
        alt:    (@options[:html] ? "<code>|</code> " : "| "),
        diff:   (@options[:html] ? "<code>-</code> " : "- "),
        star:   (@options[:html] ? "<code>*</code> " : "*"),
        plus:   (@options[:html] ? "<code>+</code> " : "+"),
        opt:    (@options[:html] ? "<code>?</code> " : "?")
      }
      lparen = (@options[:html] ? "<code>(</code> " : "(")
      rparen = (@options[:html] ? "<code>)</code> " : ")")

      case expr.first
      when :istr
        # Looses fidelity, but, oh well ...
        format_ebnf(expr.last, embedded: true)
      when :alt, :diff
        this_sep = (sep ? sep : " ") + parts[expr.first.to_sym]
        res = expr[1..-1].map {|e| format_ebnf(e, embedded: true)}.join(this_sep)
        embedded ? (lparen + res + rparen) : res
      when :star, :plus, :opt
        raise "Expected star expression to have a single operand" unless expr.length == 2
        char = parts[expr.first.to_sym]
        r = format_ebnf(expr[1], embedded: true)
        "#{r}#{char}"
      when :hex
        (@options[:html] ? %(<code class="grammar-char-escape">#{expr.last}</code>) : expr.last)
      when :range
        format_ebnf_range(expr.last)
      when :seq
        this_sep = (sep ? sep : " ")
        res = expr[1..-1].map do |e|
          format_ebnf(e, embedded: true)
        end.join(this_sep)
        embedded ? (lparen + res + rparen) : res
      when :rept
        # Expand repetition
        min, max, value = expr[1..-1]
        if min == 0 && max == 1
          format_ebnf([:opt, value], sep: sep, embedded: embedded)
        elsif min == 0 && max == '*'
          format_ebnf([:star, value], sep: sep, embedded: embedded)
        elsif min == 1 && max == '*'
          format_ebnf([:plus, value], sep: sep, embedded: embedded)
        else
          val2 = [:seq]
          while min > 0
            val2 << value
            min -= 1
            max -= 1 unless max == '*'
          end
          if max == '*'
            val2 << [:star, value]
          else
            opt = nil
            while max > 0
              opt = [:opt, opt ? [:seq, value, opt] : value]
              max -= 1
            end
            val2 << opt if opt
          end
          format_ebnf(val2, sep: sep, embedded: embedded)
        end
      else
        raise "Unknown operator: #{expr.first}"
      end
    end

    # Format a single-character string, prefering hex for non-main ASCII
    def format_ebnf_char(c)
      case c.ord
      when 0x22         then (@options[:html] ? %('<code class="grammar-literal">"</code>') : %{'"'})
      when (0x23..0x7e) then (@options[:html] ? %("<code class="grammar-literal">#{c}</code>") : %{"#{c}"})
      else                   (@options[:html] ? %(<code class="grammar-char-escape">#{escape_hex(c)}</code>) : escape_hex(c))
      end
    end

    # Format a range
    def format_ebnf_range(string)
      lbrac  =  (@options[:html] ? "<code>[</code> " : "[")
      rbrac  =  (@options[:html] ? "<code>]</code> " : "]")
      dash   = (@options[:html] ? "<code>-</code> " : "-")

      buffer = lbrac
      s = StringScanner.new(string)
      while !s.eos?
        case
        when s.scan(/\A[!"\u0024-\u007e]+/)
          buffer << (@options[:html] ? %(<code class="grammar-literal">#{s.matched}</code>) : s.matched)
        when s.scan(/\A#x\h+/)
          buffer << (@options[:html] ? %(<code class="grammar-char-escape">#{s.matched}</code>) : s.matched)
        when s.scan(/\A-/)
          buffer << dash
        else
          buffer << (@options[:html] ? %(<code class="grammar-char-escape">#{escape_hex(s.getch)}</code>) : escape_hex(s.getch))
        end
      end
      buffer + rbrac
    end

    # Escape a string, using as many UTF-8 characters as possible
    def escape(string, quote = '"')
      buffer = ""
      string.each_char do |c|
        buffer << case (u = c.ord)
        when (0x00..0x1f) then "#x%02X" % u
        when quote.ord    then "#x%02X" % u
        else                   c
        end
      end
      buffer
    end

    def escape_hex(u)
      fmt = case u.ord
      when 0x0000..0x00ff then "#x%02X"
      when 0x0100..0xffff then "#x%04X"
      else                     "#x%08X"
      end
      sprintf(fmt, u.ord)
    end

    HAML_DESC = %q(
      %table.grammar
        %tbody#grammar-productions
          - rules.each do |rule|
            %tr{id: "grammar-production-#{rule.sym}"}
              - if rule.pass?
                %td{colspan: (rule.id ? 3 : 2)}
                  %code<="@pass"
              - else
                - if rule.id
                  %td<= "[#{rule.id}]"
                %td<
                  %code<= rule.sym
                - if format == :ebnf
                  %td<= "::="
                - else
                  %td<= "="
              %td
                != yield rule
    ).gsub(/^      /, '')
  end
end
