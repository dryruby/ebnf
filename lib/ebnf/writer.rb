# -*- encoding: utf-8 -*-
require 'rdf'
require 'strscan'    unless defined?(StringScanner)
require "ostruct"
require 'unicode/types'

##
# Serialize ruleset back to EBNF
module EBNF
  class Writer
    LINE_LENGTH = 80
    LINE_LENGTH_HTML = 200

    # ASCII escape names
    ASCII_ESCAPE_NAMES = [
      "null",                 #x00
      "start of heading",     #x01
      "start of text",        #x02
      "end of text",          #x03
      "end of transmission",  #x04
      "enquiry",              #x05
      "acknowledge",          #x06
      "bell",                 #x07
      "backspace",            #x08
      "horizontal tab",       #x09
      "new line",             #x0A
      "vertical tab",         #x0B
      "form feed",            #x0C
      "carriage return",      #x0D
      "shift out",            #x0E
      "shift in",             #x0F
      "data link escape",     #x10
      "device control 1",     #x11
      "device control 2",     #x12
      "device control 3",     #x13
      "device control 4",     #x14
      "negative acknowledge", #x15
      "synchronous idle",     #x16
      "end of trans. block",  #x17
      "cancel",               #x18
      "end of medium",        #x19
      "substitute",           #x1A
      "escape",               #x1B
      "file separator",       #x1C
      "group separator",      #x1D
      "record separator",     #x1E
      "unit separator",       #x1F
      "space"                 #x20
      ]

    ##
    # Format rules to a String
    #
    # @param  [Array<Rule>] rules
    # @param [:abnf, :ebnf, :isoebnf] format (:ebnf)
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
    # @param [:abnf, :ebnf, :isoebnf] format (:ebnf)
    # @return [Object]
    def self.print(*rules, format: :ebnf)
      write($stdout, *rules, format: format)
    end

    ##
    # Write formatted rules to an IO like object
    #
    # @param  [Object] out
    # @param  [Array<Rule>] rules
    # @param [:abnf, :ebnf, :isoebnf] format (:ebnf)
    # @return [Object]
    def self.write(out, *rules, format: :ebnf)
      Writer.new(rules, out: out, format: format)
    end

    ##
    # Write formatted rules to an IO like object as HTML
    #
    # @param  [Array<Rule>] rules
    # @param [:abnf, :ebnf, :isoebnf] format (:ebnf)
    # @param [Boolean] validate (false) validate generated HTML.
    # @return [Object]
    def self.html(*rules, format: :ebnf, validate: false)
      require 'stringio' unless defined?(StringIO)
      buf = StringIO.new
      Writer.new(rules, out: buf, html: true, format: format, validate: validate)
      buf.string
    end

    ##
    # @param [Array<Rule>] rules
    # @param [:abnf, :ebnf, :isoebnf] format (:ebnf)
    # @param [Boolean] html (false) generate HTML output
    # @param [Boolean] validate (false) validate generated HTML.
    # @param [Hash{Symbol => Object}] options
    # @param [#write] out ($stdout)
    def initialize(rules, out: $stdout, html: false, format: :ebnf, validate: false, **options)
      @options = options.merge(html: html)
      return if rules.empty?

      # Determine max LHS length
      format_meth = "format_#{format}".to_sym
      max_id = rules.max_by {|r| r.id.to_s.length}.id.to_s.length
      max_sym = rules.max_by {|r| r.sym.to_s.length}.sym.to_s.length
      lhs_length = max_sym + 1
      lhs_fmt = case format
      when :abnf    then "%<sym>-#{max_sym}s = "
      when :ebnf    then "%<sym>-#{max_sym}s ::= "
      when :isoebnf then "%<sym>-#{max_sym}s = "
      end
      if format == :ebnf && max_id > 0
        lhs_fmt = "%<id>-#{max_id+2}s " + lhs_fmt
        lhs_length += max_id + 3
      end
      rhs_length = (html ? LINE_LENGTH_HTML : LINE_LENGTH) - lhs_length

      if html
        # Output as formatted HTML
        begin
          require 'erubis'
          require 'htmlentities'
          @coder = HTMLEntities.new
          eruby = Erubis::Eruby.new(ERB_DESC)
          formatted_rules = rules.map do |rule|
            if rule.kind == :terminals || rule.kind == :pass
              OpenStruct.new(id: ("@#{rule.kind}"),
                             class: :declaration,
                             sym: rule.kind,
                             assign: nil,
                             formatted: (
                               rule.kind == :terminals ?
                                 "<strong># Productions for terminals</strong>" :
                                 self.send(format_meth, rule.expr)))
            else
              formatted_expr = self.send(format_meth, rule.expr)
              # Measure text without markup
              formatted_expr_text = formatted_expr.gsub(%r{</?\w+[^>]*>}, '')
              if formatted_expr_text.length > rhs_length && (format != :abnf || rule.alt?)
                lines = []
                # Can only reasonably split apart alts
                self.send(format_meth, rule.expr, sep: "--rule-extensions--").
                split(/\s*--rule-extensions--\s*/).each_with_index do |formatted, ndx|
                  assign = case format
                  when :ebnf
                    formatted.sub!(%r{\s*<code[^>]*>\|</code>\s*}, '')
                    (ndx > 0 ? (rule.alt? ? '<code class="grammar-alt">|</code>' : '') : '::=')
                  when :abnf
                    formatted.sub!(%r{\s*<code[^>]>/</code>\s*}, '')
                    (ndx > 0 ? '<code class="grammar-alt">=/</code>' : '=')
                  else
                    formatted.sub!(%r{\s*<code[^>]>\|</code>\s*}, '')
                    (ndx > 0 ? (rule.alt? ? '<code class="grammar-alt">|</code>' : '') : '=')
                  end
                  lines << OpenStruct.new(id: ((ndx == 0 ? "[#{rule.id}]" : "") if rule.id),
                                          sym: (rule.sym if ndx == 0 || format == :abnf),
                                          class: :production,
                                          assign: assign,
                                          formatted: formatted)
                end
                if format == :isoebnf
                  lines << OpenStruct.new(assign: ';')
                end
                lines
              else
                OpenStruct.new(id: ("[#{rule.id}]" if rule.id),
                               class: :production,
                               sym: rule.sym,
                               assign: (format == :ebnf ? '::=' : '='),
                               formatted: (formatted_expr + (format == :isoebnf ? ' ;' : '')))
              end
            end
          end.flatten

          html_result = eruby.evaluate(format: format, rules: formatted_rules)

          if validate
            begin
              require 'nokogiri'
              # Validate the output HTML
              doc = ::Nokogiri::HTML5("<!DOCTYPE html>" + html_result, max_errors: 10)
              raise EncodingError, "Errors found in generated HTML:\n  " +
                doc.errors.map(&:to_s).join("\n  ") unless doc.errors.empty?
            rescue LoadError, NoMethodError
              # Skip
            end
          end

          out.write html_result
          return
        rescue LoadError
          $stderr.puts "Generating HTML requires erubis and htmlentities gems to be loaded"
        end
      end

      # Format each rule, considering the available rhs size
      rules.each do |rule|
        buffer = if rule.pass?
          "\n%-#{lhs_length-2}s      " % "@pass"
        elsif rule.kind == :terminals
          "\n%-#{lhs_length-2}s" % "@terminals"
        else
          lhs_fmt % {id: "[#{rule.id}]", sym: rule.sym}
        end
        formatted_expr = self.send(format_meth, rule.expr)
        if formatted_expr.length > rhs_length && (format != :abnf || rule.alt?)
          if format == :abnf
            # No whitespace, use =/
            self.send(format_meth, rule.expr, sep: "--rule-extensions--").
            split(/\s*--rule-extensions--\s*/).each_with_index do |formatted, ndx|
              if ndx > 0
                buffer << "\n" + lhs_fmt.sub('= ', '=/') % {id: "[#{rule.id}]", sym: rule.sym}
              end
              buffer << formatted.sub(/\s*\/\s*/, '')
            end
          else
            # Space out past "= "
            buffer << self.send(format_meth, rule.expr, sep: ("\n" + " " * (lhs_length + (rule.alt? ? 2 : 4) - (format == :ebnf ? 0 : 2))))
            buffer << ("\n" + " " * (lhs_length) + ';') if format == :isoebnf
          end
        else
          buffer << formatted_expr + (format == :isoebnf ? ' ;'  : '')
        end
        buffer << "\n\n" if [:terminals, :pass].include?(rule.kind)
        out.puts(buffer)
      end
    end

    protected

    ##
    # W3C EBNF Formatters
    ##

    # Format the expression part of a rule
    def format_ebnf(expr, sep: nil, embedded: false)
      return (@options[:html] ? %(<a href="#grammar-production-#{@coder.encode expr}">#{@coder.encode expr}</a>) : expr.to_s) if expr.is_a?(Symbol)
      if expr.is_a?(String)
        return expr.length == 1 ?
          format_ebnf_char(expr) :
          format_ebnf_string(expr, expr.include?('"') ? "'" : '"')
      end
      parts = {
        alt:    (@options[:html] ? %(<code class="grammar-alt">|</code> ) : "| "),
        diff:   (@options[:html] ? %(<code class="grammar-diff">-</code> ) : "- "),
        star:   (@options[:html] ? %(<code class="grammar-star">*</code>) : "*"),
        plus:   (@options[:html] ? %(<code class="grammar-plus">+</code>) : "+"),
        opt:    (@options[:html] ? %(<code class="grammar-opt">?</code>) : "?")
      }
      lparen = (@options[:html] ? %[<code class="grammar-paren">(</code>] : "(")
      rparen = (@options[:html] ? %[<code class="grammar-paren">)</code>] : ")")

      case expr.first
      when :istr
        # Looses fidelity, but, oh well ...
        format_ebnf(expr.last, embedded: true)
      when :alt, :diff
        this_sep = (sep ? sep : " ") + parts[expr.first.to_sym]
        res = expr[1..-1].map {|e| format_ebnf(e, embedded: true)}.join(this_sep)
        embedded ? (lparen + res + rparen) : res
      when :star, :plus, :opt
        char = parts[expr.first.to_sym]
        r = format_ebnf(expr[1], embedded: true)
        "#{r}#{char}"
      when :hex
        escape_ebnf_hex(expr.last[2..-1].hex.chr(Encoding::UTF_8))
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
      when (0x21)         then (@options[:html] ? %("<code class="grammar-literal">#{@coder.encode c}</code>") : %{"#{c}"})
      when 0x22           then (@options[:html] ? %('<code class="grammar-literal">&quot;</code>') : %{'"'})
      when (0x23..0x7e)   then (@options[:html] ? %("<code class="grammar-literal">#{@coder.encode c}</code>") : %{"#{c}"})
      when (0x80..0xFFFD) then (@options[:html] ? %("<code class="grammar-literal">#{@coder.encode c}</code>") : %{"#{c}"})
      else                     escape_ebnf_hex(c)
      end
    end

    # Format a range
    def format_ebnf_range(string)
      lbrac  =  (@options[:html] ? %(<code class="grammar-brac">[</code>) : "[")
      rbrac  =  (@options[:html] ? %(<code class="grammar-brac">]</code>) : "]")

      buffer = lbrac
      s = StringScanner.new(string)
      while !s.eos?
        case
        when s.scan(/\A[!"\u0024-\u007e]+/)
          buffer << (@options[:html] ? %(<code class="grammar-literal">#{@coder.encode s.matched}</code>) : s.matched)
        when s.scan(/\A#x\h+/)
          buffer << escape_ebnf_hex(s.matched[2..-1].hex.chr(Encoding::UTF_8))
        else
          buffer << escape_ebnf_hex(s.getch)
        end
      end
      buffer + rbrac
    end

    # Escape a string, using as many UTF-8 characters as possible
    def format_ebnf_string(string, quote = '"')
      string.each_char do |c|
        case c.ord
        when 0x00..0x19, quote.ord
          raise RangeError, "cannot format #{string.inspect} as an EBNF String: #{c.inspect} is out of range" unless
            ISOEBNF::TERMINAL_CHARACTER.match?(c)
        end
      end

      res = @options[:html] ? %(<code class="grammar-literal">#{@coder.encode(string)}</code>) : string
      res = "#{quote}#{res}#{quote}"
    end

    def escape_ebnf_hex(u)
      fmt = case u.ord
      when 0x00..0x20     then "#x%02X"
      when 0x0000..0x00ff then "#x%02X"
      when 0x0100..0xffff then "#x%04X"
      else                     "#x%08X"
      end
      char = fmt % u.ord
      if @options[:html]
        char = if u.ord <= 0x20
          %(<abbr title="#{ASCII_ESCAPE_NAMES[u.ord]}">#{@coder.encode char}</abbr>)
        elsif u.ord == 0x22
          %(<abbr title="quot">>&quot;</abbr>)
        elsif u.ord < 0x7F
          %(<abbr title="ascii '#{@coder.encode u}'">#{@coder.encode char}</abbr>)
        elsif u.ord == 0x7F
          %(<abbr title="delete">#{@coder.encode char}</abbr>)
        elsif u.ord <= 0xFF
          %(<abbr title="extended ascii '#{@coder.encode char}'">#{char}</abbr>)
        elsif (%w(Control Private-use Surrogate Noncharacter Reserved) - ::Unicode::Types.of(u)).empty?
          %(<abbr title="unicode '#{u}'">#{char}</abbr>)
        else
          %(<abbr title="unicode '#{::Unicode::Types.of(u).first}'">#{char}</abbr>)
        end
        %(<code class="grammar-char-escape">#{char}</code>)
      else
        char
      end
    end

    ##
    # ABNF Formatters
    ##

    # Format the expression part of a rule
    def format_abnf(expr, sep: nil, embedded: false, sensitive: true)
      return (@options[:html] ? %(<a href="#grammar-production-#{@coder.encode expr}">#{@coder.encode expr}</a>) : expr.to_s) if expr.is_a?(Symbol)
      if expr.is_a?(String)
        if expr.length == 1
          return format_abnf_char(expr)
        elsif expr.start_with?('%')
          # Already encoded
          return expr
        elsif expr =~ /"/
          # Split into segments
          segments = expr.split('"')

          return format_abnf_char(expr) if segments.empty?

          seq = segments.inject([]) {|memo, s| memo.concat([[:hex, "#x22"], s])}[1..-1]
          seq.unshift(:seq)
          return format_abnf(seq, sep: nil, embedded: false)
        else
          return (@options[:html] ? %("<code class="grammar-literal">#{@coder.encode expr}</code>") : %("#{expr}"))
        end
      end
      parts = {
        alt:    (@options[:html] ? %(<code class="grammar-alt">/</code>) : "/ "),
        star:   (@options[:html] ? %(<code class="grammar-star">*</code>) : "*"),
        plus:   (@options[:html] ? %(<code class="grammar-plus">+</code>) : "1*"),
        opt:    (@options[:html] ? %(<code class="grammar-opt">?</code>) : "?")
      }
      lbrac = (@options[:html] ? %(<code class="grammar-brac">[</code>) : "[")
      rbrac = (@options[:html] ? %(<code class="grammar-brac">]</code>) : "]")
      lparen = (@options[:html] ? %[<code class="grammar-paren">(</code>] : "(")
      rparen = (@options[:html] ? %[<code class="grammar-paren">)</code>] : ")")

      case expr.first
      when :istr
        # FIXME: if string part is segmented, need to do something different
        format_abnf(expr.last, embedded: true, sensitive: false)
      when :alt
        this_sep = (sep ? sep : " ") + parts[expr.first.to_sym]
        res = expr[1..-1].map {|e| format_abnf(e, embedded: true)}.join(this_sep)
        embedded ? (lparen + res + rparen) : res
      when :diff
        raise RangeError, "ABNF does not support the diff operator"
      when :opt
        char = parts[expr.first.to_sym]
        r = format_abnf(expr[1], embedded: true)
        "#{lbrac}#{r}#{rbrac}"
      when :plus, :star
        char = parts[expr.first.to_sym]
        r = format_abnf(expr[1], embedded: true)
        "#{char}#{r}"
      when :hex
        escape_abnf_hex(expr.last[2..-1].hex.chr)
      when :range
        # Returns an [:alt] or [:not [:alt]] if composed of multiple sequences
        # Note: ABNF does not support the `not` operator
        res = format_abnf_range(expr.last)
        res.is_a?(Array) ?
          format_abnf(res, embedded: true) :
          res
      when :seq
        this_sep = (sep ? sep : " ")
        res = expr[1..-1].map do |e|
          format_abnf(e, embedded: true)
        end.join(this_sep)
        embedded ? (lparen + res + rparen) : res
      when :rept
        # Expand repetition
        min, max, value = expr[1..-1]
        r = format_abnf(value, embedded: true)
        if min == max
          "#{min}#{r}"
        elsif min == 0 && max == '*'
          "#{parts[:star]}#{r}"
        elsif min > 0 && max == '*'
          "#{min}#{parts[:star]}#{r}"
        elsif min == 0
          "#{parts[:star]}#{max}#{r}"
        else
          "#{min}#{parts[:star]}#{max}#{r}"
        end
      else
        raise "Unknown operator: #{expr.first}"
      end
    end

    # Format a single-character string, prefering hex for non-main ASCII
    def format_abnf_char(c)
      if /[\x20-\x21\x23-\x7E]/.match?(c)
        @options[:html] ? %("<code class="grammar-literal">#{@coder.encode c}</code>") : c.inspect
      else
        escape_abnf_hex(c)
      end
    end

    # Format a range
    #
    # Presumes range has already been validated
    def format_abnf_range(string)
      alt, o_dash = [:alt], false

      raise RangeError, "cannot format #{string.inspect} an ABNF range" if string.start_with?('^')

      if string.end_with?('-')
        o_dash = true
        string = string[0..-2]
      end

      scanner = StringScanner.new(string)
      hexes, deces = [], []
      in_range = false
      # Build op (alt) from different ranges/enums
      while !scanner.eos?
        if hex = scanner.scan(Terminals::HEX)
          # Append any decimal values
          alt << "%d" + deces.join(".") unless deces.empty?
          deces = []
          hex = hex.upcase

          if in_range
            # Add "." sequences for any previous hexes
            alt << "%x" + hexes[0..-2].join(".") if hexes.length > 1
            alt << "%x#{hexes.last}-#{hex[2..-1]}"
            in_range, hexes = false, []
          else
            hexes << hex[2..-1]
          end
        elsif dec = scanner.scan(Terminals::R_CHAR)
          # Append any hexadecimal values
          alt << "%x" + hexes.join(".") unless hexes.empty?
          hexes = []

          if in_range
            # Add "." sequences for any previous hexes
            alt << "%d" + deces[0..-2].join(".") if deces.length > 1
            alt << "%d#{deces.last}-#{dec.codepoints.first}"
            in_range, deces = false, []
          else
            deces << dec.codepoints.first.to_s
          end
        end

        in_range = true if scanner.scan(/\-/)
      end

      deces << '45' if o_dash

      # Append hexes and deces as "." sequences (should be only one)
      alt << "%d" + deces.join(".") unless deces.empty?
      alt << "%x" + hexes.join(".") unless hexes.empty?

      # FIXME: HTML abbreviations?
      if alt.length == 2
        # Just return the range or enum
        alt.last
      else
        # Return the alt, which will be further formatted
        alt
      end
    end

    def escape_abnf_hex(u)
      fmt = case u.ord
      when 0x0000..0x00ff then "%02X"
      when 0x0100..0xffff then "%04X"
      else                     "%08X"
      end
      char =  "%x" + (fmt % u.ord).upcase
      if @options[:html]
        if u.ord <= 0x20
          char = %(<abbr title="#{ASCII_ESCAPE_NAMES[u.ord]}">#{@coder.encode char}</abbr>)
        elsif u.ord == 0x22
          %(<abbr title="quot">>&quot;</abbr>)
        elsif u.ord < 0x7F
          char = %(<abbr title="ascii '#{u}'">#{@coder.encode char}</abbr>)
        elsif u.ord == 0x7F
          char = %(<abbr title="delete">#{@coder.encode char}</abbr>)
        elsif u.ord <= 0xFF
          char = %(<abbr title="extended ascii '#{u}'">#{char}</abbr>)
        else
          char = %(<abbr title="unicode '#{u.unicode_normaliz}'">#{char}</abbr>)
        end
        %(<code class="grammar-char-escape">#{char}</code>)
      else
        char
      end
    end

    ##
    # ISO EBNF Formatters
    ##

    # Format the expression part of a rule
    def format_isoebnf(expr, sep: nil, embedded: false)
      return (@options[:html] ? %(<a href="#grammar-production-#{@coder.encode expr}">#{@coder.encode expr}</a>) : expr.to_s) if expr.is_a?(Symbol)
      if expr.is_a?(String)
        expr = expr[2..-1].hex.chr if expr =~ /\A#x\h+/
        expr.chars.each do |c|
          raise RangeError, "cannot format #{expr.inspect} as an ISO EBNF String: #{c.inspect} is out of range" unless
            ISOEBNF::TERMINAL_CHARACTER.match?(c)
        end
        if expr =~ /"/
          return (@options[:html] ? %('<code class="grammar-literal">#{@coder.encode expr}</code>') : %('#{expr}'))
        else
          return (@options[:html] ? %("<code class="grammar-literal">#{@coder.encode expr}</code>") : %("#{expr}"))
        end
      end
      parts = {
        alt:    (@options[:html] ? %(<code class="grammar-alt">|</code> ) : "| "),
        diff:   (@options[:html] ? %(<code class="grammar-diff">-</code> ) : "- "),
      }
      lparen = (@options[:html] ? %[<code class="grammar-paren">(</code>] : "(")
      rparen = (@options[:html] ? %[<code class="grammar-paren">)</code>] : ")")

      case expr.first
      when :istr
        # Looses fidelity, but, oh well ...
        format_isoebnf(expr.last, embedded: true)
      when :alt, :diff
        this_sep = (sep ? sep : " ") + parts[expr.first.to_sym]
        res = expr[1..-1].map {|e| format_isoebnf(e, embedded: true)}.join(this_sep)
        embedded ? (lparen + res + rparen) : res
      when :opt
        r = format_isoebnf(expr[1], embedded: true)
        "[#{r}]"
      when :star
        r = format_isoebnf(expr[1], embedded: true)
        "{#{r}}"
      when :plus
        r = format_isoebnf(expr[1], embedded: true)
        "#{r}, {#{r}}"
      when :hex
        format_isoebnf(expr[1], embedded: true)
      when :range
        res = format_isoebnf_range(expr.last)
        res.is_a?(Array) ?
          format_isoebnf(res, embedded: true) :
          res
      when :seq
        this_sep = "," + (sep ? sep : " ")
        res = expr[1..-1].map do |e|
          format_isoebnf(e, embedded: true)
        end.join(this_sep)
        embedded ? (lparen + res + rparen) : res
      when :rept
        # Expand repetition
        min, max, value = expr[1..-1]
        if min == 0 && max == 1
          format_isoebnf([:opt, value], sep: sep, embedded: embedded)
        elsif min == 0 && max == '*'
          format_isoebnf([:star, value], sep: sep, embedded: embedded)
        elsif min == 1 && max == '*'
          format_isoebnf([:plus, value], sep: sep, embedded: embedded)
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
          format_isoebnf(val2, sep: sep, embedded: embedded)
        end
      else
        raise "Unknown operator: #{expr.first}"
      end
    end

    # Format a range
    # Range is formatted as a aliteration of characters
    def format_isoebnf_range(string)
      chars = []
      o_dash = false

      raise RangeError, "cannot format #{string.inspect} an ABNF range" if string.start_with?('^')

      if string.end_with?('-')
        o_dash = true
        string = string[0..-2]
      end

      scanner = StringScanner.new(string)
      in_range = false
      # Build chars from different ranges/enums
      while !scanner.eos?
        char = if hex = scanner.scan(Terminals::HEX)
          hex[2..-1].hex.ord.char(Encoding::UTF_8)
        else scanner.scan(Terminals::R_CHAR)
        end
        raise RangeError, "cannot format #{string.inspect} as an ISO EBNF Aliteration: #{char.inspect} is out of range" unless
          char && ISOEBNF::TERMINAL_CHARACTER.match?(char)

        if in_range
          # calculate characters from chars.last to this char
          raise RangeError, "cannot format #{string.inspect} as an ISO EBNF Aliteration" unless chars.last < char
          chars.concat (chars.last..char).to_a[1..-1]
          in_range = false
        else
          chars << char
        end

        in_range = true if scanner.scan(/\-/)
      end

      chars << '-' if o_dash

      # Possibly only a single character (no character?)
      chars.length == 1 ? chars.last.inspect : chars.unshift(:alt)
    end

    ERB_DESC = %(<!-- Generated with ebnf version #{EBNF::VERSION}. See https://github.com/dryruby/ebnf. -->\n) +
      %q(<table class="grammar">
        <tbody id="grammar-productions" class="<%= @format %>">
          <% for rule in @rules %>
          <tr<%= %{ id="grammar-#{rule[:class]}-#{rule.sym}"} unless %w(=/ |).include?(rule.assign) || rule.sym.nil?%>>
            <% if rule.id %>
            <td<%= " colspan=2" unless rule.sym %>><%= rule.id %></td>
            <% end %>
            <% if rule.sym %>
            <td><code><%== (rule.sym unless rule.class == :declaration) %></code></td>
            <% end %>
            <td><%= rule.assign %></td>
            <td><%= rule.formatted %></td>
          </tr>
          <% end %>
        </tbody>
      </table>
    ).gsub(/^      /, '')
  end
end
