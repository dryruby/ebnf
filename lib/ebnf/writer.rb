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
    # @return [Object]
    def self.string(*rules)
      require 'stringio' unless defined?(StringIO)
      buf = StringIO.new
      write(buf, *rules)
      buf.string
    end

    ##
    # Format rules to $stdout
    #
    # @param  [Array<Rule>] rules
    # @return [Object]
    def self.print(*rules)
      write($stdout, *rules)
    end

    ##
    # Write formatted rules to an IO like object
    #
    # @param  [Object] out
    # @param  [Array<Rule>] rules
    # @return [Object]
    def self.write(out, *rules)
      Writer.new(rules, out: out)
    end

    ##
    # Write formatted rules to an IO like object as HTML
    #
    # @param  [Array<Rule>] rules
    # @return [Object]
    def self.html(*rules)
      require 'stringio' unless defined?(StringIO)
      buf = StringIO.new
      Writer.new(rules, out: buf, html: true)
      buf.string
    end

    ##
    # @param [Array<Rule>] rules
    # @param [Hash{Symbol => Object}] options
    # @option options [Symbol] :format
    # @option options [#write] :out ($stdout)
    # @option options [Boolean] :html (false)
    #   Format as HTML
    def initialize(rules, options = {})
      @options = options.dup
      out = options.fetch(:out, $stdio)
      #fmt = options.fetch(:format, :ebnf)

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

      if @options[:html]
        # Output as formatted HTML
        require 'haml'
        html = Haml::Engine.new(HAML_DESC).render(self, rules: rules) do |rule|
          formatted_expr = format(rule.expr)
          formatted_expr.length > rhs_length ? format(rule.expr, "\n") : formatted_expr
        end
        out.write html
        return
      end

      # Format each rule, considering the available rhs size
      rules.each do |rule|
        buffer = if rule.pass?
          "%-#{lhs_length-2}s" % "@pass"
        else
          lhs_fmt % {:id => "[#{rule.id}]", :sym => rule.sym}
        end
        formatted_expr = format(rule.expr)
        if formatted_expr.length > rhs_length
          buffer << format(rule.expr, ("\n" + " " * lhs_length))
        else
          buffer << formatted_expr
        end
        out.puts(buffer)
      end
    end

    protected
    # Format the expression part of a rule
    def format(expr, sep = nil)
      return (@options[:html] ? %(<a href="#grammar-production-#{expr}">#{expr}</a>) : expr.to_s) if expr.is_a?(Symbol)
      if expr.is_a?(String)
        if expr.length == 1
          return format_char(expr)
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
      when :alt, :diff
        this_sep = (sep ? sep : " ") + parts[expr.first.to_sym]
        expr[1..-1].map {|e| format(e)}.join(this_sep)
      when :star, :plus, :opt
        raise "Expected star expression to have a single operand" unless expr.length == 2
        char = parts[expr.first.to_sym]
        r = format(expr[1])
        (r.start_with?("(") || Array(expr[1]).length == 1) ? "#{r}#{char}" : "(#{r})#{char}"
      when :hex
        (@options[:html] ? %(<code class="grammar-char-escape">#{expr.last}</code>) : expr.last)
      when :range
        format_range(expr.last)
      when :seq
        this_sep = (sep ? sep : " ")
        expr[1..-1].map {|e| r = format(e); Array(e).length > 2 ? "#{lparen}#{r}#{rparen}" : r}.join(this_sep)
      else
        raise "Unknown operator: #{expr.first}"
      end
    end

    # Format a single-character string, prefering hex for non-main ASCII
    def format_char(c)
      case c.ord
      when 0x22         then (@options[:html] ? %('<code class="grammar-literal">"</code>') : %{'"'})
      when (0x23..0x7e) then (@options[:html] ? %("<code class="grammar-literal">#{c}</code>") : %{"#{c}"})
      else                   (@options[:html] ? %(<code class="grammar-char-escape">#{escape_hex(c)}</code>) : escape_hex(c))
      end
    end

    # Format a range
    def format_range(string)
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
                %td{colspan: 3}
                  %code<="@pass"
              - else
                %td<= "[#{rule.id}]"
                %td<
                  %code<= rule.sym
                %td<= "::="
              %td
                != yield rule
    ).gsub(/^      /, '')
  end
end
