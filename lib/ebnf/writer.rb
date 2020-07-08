# -*- encoding: utf-8 -*-
require 'rdf'
require 'strscan'    unless defined?(StringScanner)
require "ostruct"

##
# Serialize ruleset back to EBNF
module EBNF
  class Writer
    LINE_LENGTH = 80

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
    ].freeze

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
    # @param [:abnf, :ebnf, :isoebnf] format (:ebnf)
    # @option options [Symbol] format
    # @option options [Boolean] html (false)
    def initialize(rules, out: $stdout, html: false, format: :ebnf, **options)
      @options = options.merge(html: html)
      return if rules.empty?

      # Determine max LHS length
      format_meth = "format_#{format}".to_sym
      max_id = rules.max_by {|r| r.id.to_s.length}.id.to_s.length
      max_sym = rules.max_by {|r| r.sym.to_s.length}.sym.to_s.length
      lhs_length = max_sym + 1
      lhs_fmt = "%<sym>-#{max_sym}s #{format == :ebnf ? '::=' : '='} "
      if format == :ebnf && max_id > 0
        lhs_fmt = "%<id>-#{max_id+2}s " + lhs_fmt
        lhs_length += max_id + 3
      end
      rhs_length = LINE_LENGTH - lhs_length

      if html
        # Output as formatted HTML
        begin
          require 'erubis'
          eruby = Erubis::Eruby.new(ERB_DESC)
          formatted_rules = rules.map do |rule|
            formatted_expr = self.send(format_meth, rule.expr)
            formatted_expr.length > rhs_length ? self.send(format_meth, rule.expr, sep: "\n") : formatted_expr
            OpenStruct.new(id: rule.id, sym: rule.sym, pass: rule.pass?, formatted: formatted_expr)
          end
          out.write eruby.evaluate(format: format, rules: formatted_rules)
          return
        rescue LoadError
          $stderr.puts "Generating HTML requires erubis gem to be loaded"
        end
      end

      # Format each rule, considering the available rhs size
      rules.each do |rule|
        buffer = if rule.pass?
          "%-#{lhs_length-2}s" % "@pass"
        else
          lhs_fmt % {id: "[#{rule.id}]", sym: rule.sym}
        end
        formatted_expr = self.send(format_meth, rule.expr)
        if formatted_expr.length > rhs_length
          # Space out past "= "
          buffer << self.send(format_meth, rule.expr, sep: ("\n" + " " * (lhs_length + (format == :ebnf ? 4 : 2))))
        else
          buffer << formatted_expr
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
          return format_ebnf_hex(expr[2..-1].hex.chr)
        elsif expr =~ /"/
          return (@options[:html] ? %('<code class="grammar-literal">#{escape_ebnf(expr, "'")}</code>') : %('#{escape_ebnf(expr, "'")}'))
        else
          return (@options[:html] ? %("<code class="grammar-literal">#{escape_ebnf(expr, '"')}</code>") : %("#{escape_ebnf(expr, '"')}"))
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
      when (0x21)         then (@options[:html] ? %("<code class="grammar-literal">#{c}</code>") : %{"#{c}"})
      when 0x22           then (@options[:html] ? %('<code class="grammar-literal">"</code>') : %{'"'})
      when (0x23..0x7e)   then (@options[:html] ? %("<code class="grammar-literal">#{c}</code>") : %{"#{c}"})
      when (0x80..0xFFFD) then (@options[:html] ? %("<code class="grammar-literal">#{c}</code>") : %{"#{c}"})
      else                     escape_ebnf_hex(c)
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
          buffer << escape_ebnf_hex(s.matched[2..-1].hex.chr(Encoding::UTF_8))
        when s.scan(/\A-/)
          buffer << dash
        else
          buffer << escape_ebnf_hex(s.getch)
        end
      end
      buffer + rbrac
    end

    # Escape a string, using as many UTF-8 characters as possible
    def escape_ebnf(string, quote = '"')
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

    def escape_ebnf_hex(u)
      fmt = case u.ord
      when 0x00..0x20     then "#x%02X"
      when 0x0000..0x00ff then "#x%02X"
      when 0x0100..0xffff then "#x%04X"
      else                     "#x%08X"
      end
      char = fmt % u.ord
      if @options[:html]
        if u.ord <= 0x20
          char = %(<abbr title="#{ASCII_ESCAPE_NAMES[u.ord]}">#{char}</abbr>)
        elsif u.ord == 0x7F
          char = %(<abbr title="delete">#{char}</abbr>)
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
      return (@options[:html] ? %(<a href="#grammar-production-#{expr}">#{expr}</a>) : expr.to_s) if expr.is_a?(Symbol)
      if expr.is_a?(String)
        if expr.length == 1
          return format_abnf_char(expr)
        elsif expr =~ /"/
          # Split into segments
          segments = expr.split('"')

          return format_abnf_char(expr) if segments.empty?

          seq = segments.inject([]) {|memo, s| memo.concat([[:hex, "#x22"], s])}[1..-1]
          seq.unshift(:seq)
          return format_abnf(seq, sep: nil, embedded: false)
        elsif expr.match?(/[\x00-\x1F\u{7F}-\u{10FFFF}]/)
          # Express using %d notation
          return format_abnf_range(expr)
        else
          return (@options[:html] ? %("<code class="grammar-literal">#{'%s' if sensitive}#{expr}</code>") : %(#{'%s' if sensitive}"#{expr}"))
        end
      end
      parts = {
        alt:    (@options[:html] ? "<code>|</code> " : "| "),
        star:   (@options[:html] ? "<code>*</code> " : "*"),
        plus:   (@options[:html] ? "<code>+</code> " : "1*"),
        opt:    (@options[:html] ? "<code>?</code> " : "?")
      }
      lbrac = (@options[:html] ? "<code>[</code> " : "[")
      rbrac = (@options[:html] ? "<code>]</code> " : "]")
      lparen = (@options[:html] ? "<code>(</code> " : "(")
      rparen = (@options[:html] ? "<code>)</code> " : ")")

      case expr.first
      when :istr
        # FIXME: if string part is segmented, need to do something different
        format_abnf(expr.last, embedded: true, sensitive: false)
      when :alt
        this_sep = (sep ? sep : " ") + parts[expr.first.to_sym]
        res = expr[1..-1].map {|e| format_abnf(e, embedded: true)}.join(this_sep)
        embedded ? (lparen + res + rparen) : res
      when :diff
        raise "ABNF does not support the diff operator"
      when :opt
        char = parts[expr.first.to_sym]
        r = format_abnf(expr[1], embedded: true)
        "#{lbrac}#{r}#{rbrac}"
      when :plus, :star
        char = parts[expr.first.to_sym]
        r = format_abnf(expr[1], embedded: true)
        "#{char}#{r}"
      when :hex
        hex = expr.last.sub('#', '%')
        (@options[:html] ? %(<code class="grammar-char-escape">#{hex}</code>) : hex)
      when :range
        format_abnf_range(expr.last)
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
        c.inspect
      else
        escape_abnf_hex(c)
      end
    end

    # Format a range
    # FIXME: O_RANGE
    def format_abnf_range(string)
      if string.include?('-') && !string.end_with?('-')
        # Might include multiple ranges
        # #x01-#x03#x05-#x06
        # a-bc-d
        dash = (@options[:html] ? "<code>-</code> " : "-")
        # Split into separate range segments
        if string.start_with?('#x')
          ranges = []
          scanner = StringScanner.new(string)
          while !scanner.eos?
            ranges << scanner.scan(/#x\h+-#x\h+/)
          end
          ranges.map {|range|"%x" + range.gsub('#x', '').sub('-', dash)}.join(" / ")
        else
          '%d' + string.gsub(/[^-]/) {|c| c.ord}
        end
      else
        if string.start_with?('#x')
          "%x" + string.split('#x').join('.')
        else
          "%d" + string.chars.map(&:ord).join(".")
        end
      end
    end

    def escape_abnf_hex(u)
      fmt = case u.ord
      when 0x0000..0x00ff then "#x%02X"
      when 0x0100..0xffff then "#x%04X"
      else                     "#x%08X"
      end
      char =  "%x" + (fmt % u.ord)
      if @options[:html]
        if u.ord <= 0x20
          char = %(<abbr title="#{ASCII_ESCAPE_NAMES[u.ord]}">#{char}</abbr>)
        elsif u.ord == 0x7F
          char = %(<abbr title="delete">#{char}</abbr>)
        else
          char = %(<abbr title="#{u.ord.char(Encoding::UTF_8)}">#{char}</abbr>)
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
      return (@options[:html] ? %(<a href="#grammar-production-#{expr}">#{expr}</a>) : expr.to_s) if expr.is_a?(Symbol)
      if expr.is_a?(String)
        expr = expr[2..-1].hex.chr if expr =~ /\A#x\h+/
        expr.chars.each do |c|
          raise RangeError, "cannot format #{expr.inspect} as an ISO EBNF String: #{c.inspect} is out of range" unless
            ISOEBNF::TERMINAL_CHARACTER.match?(c)
        end
        if expr =~ /"/
          return (@options[:html] ? %('<code class="grammar-literal">#{expr}</code>') : %('#{expr}'))
        else
          return (@options[:html] ? %("<code class="grammar-literal">#{expr}</code>") : %("#{expr}"))
        end
      end
      parts = {
        alt:    (@options[:html] ? "<code>|</code> " : "| "),
        diff:   (@options[:html] ? "<code>-</code> " : "- "),
      }
      lparen = (@options[:html] ? "<code>(</code> " : "(")
      rparen = (@options[:html] ? "<code>)</code> " : ")")

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
        format_isoebnf_range(expr.last)
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
        elsif min > 0 && min == max
          "#{min}*" + format_isoebnf(value, sep: sep, embedded: embedded)
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
    # FIXME: O_RANGE
    def format_isoebnf_range(string)
      chars = []
      scanner = StringScanner.new(string)
      if string.include?('-') && !string.end_with?('-')
        ranges = []
        # Might include multiple ranges
        # #x01-#x03#x05-#x06
        # a-bc-d
        # Split into separate range segments
        if string.start_with?('#x')
          while !scanner.eos?
            ranges << scanner.scan(/#x\h+-#x\h+/)
          end
          ranges.each do |range|
            first, last = range.split('-').map {|h| h[2..-1].hex.ord}
            while first <= last
              c = first.chr(Encoding::UTF_8)
              raise RangeError, "cannot format #{string.inspect} as an ISO EBNF String: #{c.inspect} is out of range" unless
                ISOEBNF::TERMINAL_CHARACTER.match?(c)
              chars << c
              first += 1
            end
          end
        else
          while !scanner.eos?
            r = scanner.scan(/.-./)
            require 'byebug'; byebug unless r
            ranges << r
          end
          ranges.each do |range|
            first, last = range.split('-').map {|c| c.ord}
            while first <= last
              c = first.chr(Encoding::UTF_8)
              raise RangeError, "cannot format #{string.inspect} as an ISO EBNF String: #{c.inspect} is out of range" unless
                ISOEBNF::TERMINAL_CHARACTER.match?(c)
              chars << c
              first += 1
            end
          end
        end
      else
        while !scanner.eos?
          c = if hex = scanner.scan(/#x\h+/)
            hex[2..-1].hex.ord.chr(Encoding::UTF_8)
          else
            scanner.scan(/./)
          end
        end
        raise RangeError, "cannot format #{string.inspect} as an ISO EBNF String: #{c.inspect} is out of range" unless
          ISOEBNF::TERMINAL_CHARACTER.match?(c)
        chars << c
      end
    end

    ERB_DESC = %q(
      <table class="grammar">
        <tbody id="grammar-productions" class="<%= @format %>">
          <% for rule in @rules %>
            <tr id="grammar-production-<%=rule.sym%>">
              <% if rule.pass %>
              <td colspan="<%=rule.id ? 4 : 3%>"><code>@pass</code></td>
              <% else %>
              <% if rule.id %>
              <td>[<%==rule.id%>]</td>
              <% end %>
              <td><code><%== rule.sym %></td>
              <td><%= @format == :ebnf ? '::=' : '='%></td>
              <% end %>
              <td><%= rule.formatted %></td>
            </tr>
          <% end %>
        </tbody>
      </table>
    ).gsub(/^      /, '')
  end
end
