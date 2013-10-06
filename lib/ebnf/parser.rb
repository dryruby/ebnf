module EBNF
  module Parser
    ##
    # Iterate over rule strings.
    # a line that starts with '\[' or '@' starts a new rule
    #
    # @param [StringScanner] scanner
    # @yield rule_string
    # @yieldparam [String] rule_string
    def eachRule(scanner)
      cur_lineno = 1
      r = ''
      until scanner.eos?
        case
        when s = scanner.scan(%r(\s+)m)
          # Eat whitespace
          cur_lineno += s.count("\n")
          #debug("eachRule(ws)") { "[#{cur_lineno}] #{s.inspect}" }
        when s = scanner.scan(%r(/\*([^\*]|\*[^\/])*\*/)m)
          # Eat comments
          debug("eachRule(comment)") { "[#{cur_lineno}] #{s.inspect}" }
        when s = scanner.scan(%r((#|//).*$))
          # Eat comments
          cur_lineno += s.count("\n")
          debug("eachRule(comment)") { "[#{cur_lineno}] #{s.inspect}" }
        when s = scanner.scan(%r(^@terminals))
          #debug("eachRule(@terminals)") { "[#{cur_lineno}] #{s.inspect}" }
          yield(r) unless r.empty?
          @lineno = cur_lineno
          yield(s)
          r = ''
        when s = scanner.scan(/@pass/)
          # Found rule start, if we've already collected a rule, yield it
          #debug("eachRule(@pass)") { "[#{cur_lineno}] #{s.inspect}" }
          yield r unless r.empty?
          @lineno = cur_lineno
          r = s
        when s = scanner.scan(/\[(?=[\w\.]+\])/)
          # Found rule start, if we've already collected a rule, yield it
          yield r unless r.empty?
          #debug("eachRule(rule)") { "[#{cur_lineno}] #{s.inspect}" }
          @lineno = cur_lineno
          r = s
        else
          # Collect until end of line, or start of comment
          s = scanner.scan_until(%r((?:/\*)|$)m)
          cur_lineno += s.count("\n")
          #debug("eachRule(rest)") { "[#{cur_lineno}] #{s.inspect}" }
          r += s
        end
      end
      yield r unless r.empty?
    end
  
    ##
    # Parse a rule into a rule number, a symbol and an expression
    #
    # @param [String] rule
    # @return [Rule]
    def ruleParts(rule)
      num_sym, expr = rule.split('::=', 2).map(&:strip)
      num, sym = num_sym.split(']', 2).map(&:strip)
      num = num[1..-1]
      r = Rule.new(sym && sym.to_sym, num, expression(expr).first, :ebnf => self)
      debug("ruleParts") { r.inspect }
      r
    end

    ##
    # Parse a string into an expression tree and a remaining string
    #
    # @example
    #     >>> expression("a b c")
    #     ((seq a b c) '')
    #     
    #     >>> expression("a? b+ c*")
    #     ((seq (opt a) (plus b) (star c)) '')
    #     
    #     >>> expression(" | x xlist")
    #     ((alt (seq) (seq x xlist)) '')
    #     
    #     >>> expression("a | (b - c)")
    #     ((alt a (diff b c)) '')
    #     
    #     >>> expression("a b | c d")
    #     ((alt (seq a b) (seq c d)) '')
    #     
    #     >>> expression("a | b | c")
    #     ((alt a b c) '')
    #     
    #     >>> expression("a) b c")
    #     (a ' b c')
    #     
    #     >>> expression("BaseDecl? PrefixDecl*")
    #     ((seq (opt BaseDecl) (star PrefixDecl)) '')
    #     
    #     >>> expression("NCCHAR1 | diff | [0-9] | #x00B7 | [#x0300-#x036F] | \[#x203F-#x2040\]")
    #     ((alt NCCHAR1 diff
    #           (range '0-9')
    #           (hex '#x00B7')
    #           (range '#x0300-#x036F')
    #           (range, '#x203F-#x2040')) '')
    #     
    # @param [String] s
    # @return [Array]
    def expression(s)
      debug("expression") {"(#{s.inspect})"}
      e, s = depth {alt(s)}
      debug {"=> alt returned #{[e, s].inspect}"}
      unless s.to_s.empty?
        t, ss = depth {terminal(s)}
        debug {"=> terminal returned #{[t, ss].inspect}"}
        return [e, ss] if t.is_a?(Array) && t.first == :")"
      end
      [e, s]
    end
  
    ##
    # Parse alt
    #     >>> alt("a | b | c")
    #     ((alt a b c) '')
    # @param [String] s
    # @return [Array]
    def alt(s)
      debug("alt") {"(#{s.inspect})"}
      args = []
      while !s.to_s.empty?
        e, s = depth {seq(s)}
        debug {"=> seq returned #{[e, s].inspect}"}
        if e.to_s.empty?
          break unless args.empty?
          e = [:seq, []] # empty sequence
        end
        args << e
        unless s.to_s.empty?
          t, ss = depth {terminal(s)}
          break unless t[0] == :alt
          s = ss
        end
      end
      args.length > 1 ? [args.unshift(:alt), s] : [e, s]
    end
  
    ##
    # parse seq
    #
    #     >>> seq("a b c")
    #     ((seq a b c) '')
    #     
    #     >>> seq("a b? c")
    #     ((seq a (opt b) c) '')
    def seq(s)
      debug("seq") {"(#{s.inspect})"}
      args = []
      while !s.to_s.empty?
        e, ss = depth {diff(s)}
        debug {"=> diff returned #{[e, ss].inspect}"}
        unless e.to_s.empty?
          args << e
          s = ss
        else
          break;
        end
      end
      if args.length > 1
        [args.unshift(:seq), s]
      elsif args.length == 1
        args + [s]
      else
        ["", s]
      end
    end
  
    ##
    # parse diff
    # 
    #     >>> diff("a - b")
    #     ((diff a b) '')
    def diff(s)
      debug("diff") {"(#{s.inspect})"}
      e1, s = depth {postfix(s)}
      debug {"=> postfix returned #{[e1, s].inspect}"}
      unless e1.to_s.empty?
        unless s.to_s.empty?
          t, ss = depth {terminal(s)}
          debug {"diff #{[t, ss].inspect}"}
          if t.is_a?(Array) && t.first == :diff
            s = ss
            e2, s = primary(s)
            unless e2.to_s.empty?
              return [[:diff, e1, e2], s]
            else
              error("diff", "Syntax Error")
              raise "Syntax Error"
            end
          end
        end
      end
      [e1, s]
    end
  
    ##
    # parse postfix
    # 
    #     >>> postfix("a b c")
    #     (a ' b c')
    #     
    #     >>> postfix("a? b c")
    #     ((opt, a) ' b c')
    def postfix(s)
      debug("postfix") {"(#{s.inspect})"}
      e, s = depth {primary(s)}
      debug {"=> primary returned #{[e, s].inspect}"}
      return ["", s] if e.to_s.empty?
      if !s.to_s.empty?
        t, ss = depth {terminal(s)}
        debug {"=> #{[t, ss].inspect}"}
        if t.is_a?(Array) && [:opt, :star, :plus].include?(t.first)
          return [[t.first, e], ss]
        end
      end
      [e, s]
    end

    ##
    # parse primary
    # 
    #     >>> primary("a b c")
    #     (a ' b c')
    def primary(s)
      debug("primary") {"(#{s.inspect})"}
      t, s = depth {terminal(s)}
      debug {"=> terminal returned #{[t, s].inspect}"}
      if t.is_a?(Symbol) || t.is_a?(String)
        [t, s]
      elsif %w(range hex).map(&:to_sym).include?(t.first)
        [t, s]
      elsif t.first == :"("
        e, s = depth {expression(s)}
        debug {"=> expression returned #{[e, s].inspect}"}
        [e, s]
      else
        ["", s]
      end
    end
  
    ##
    # parse one terminal; return the terminal and the remaining string
    # 
    # A terminal is represented as a tuple whose 1st item gives the type;
    # some types have additional info in the tuple.
    # 
    # @example
    #     >>> terminal("'abc' def")
    #     ('abc' ' def')
    #     
    #     >>> terminal("[0-9]")
    #     ((range '0-9') '')
    #     >>> terminal("#x00B7")
    #     ((hex '#x00B7') '')
    #     >>> terminal ("\[#x0300-#x036F\]")
    #     ((range '#x0300-#x036F') '')
    #     >>> terminal("\[^<>'{}|^`\]-\[#x00-#x20\]")
    #     ((range "^<>'{}|^`") '-\[#x00-#x20\]')
    def terminal(s)
      s = s.strip
      case m = s[0,1]
      when '"', "'" # STRING1 or STRING2 Terminated by line-end or whitespace
        l, s = s[1..-1].split(m.rstrip  , 2)
        [l, s]
      when '[' # ENUM, RANGE, O_ENUM, or O_RANGE
        l, s = s[1..-1].split(/(?<=[^\\])\]/, 2)
        [[:range, l], s]
      when '#' # HEX
        s.match(/(#\w+)(.*)$/)
        l, s = $1, $2
        [[:hex, l], s]
      when /[\w\.]/ # SYMBOL
        s.match(/(\w+)(.*)$/)
        l, s = $1, $2
        [l.to_sym, s]
      when '@' # @pass or @terminals
        s.match(/@(#\w+)(.*)$/)
        l, s = $1, $2
        [[:"@", l], s]
      when '-'
        [[:diff], s[1..-1]]
      when '?'
        [[:opt], s[1..-1]]
      when '|'
        [[:alt], s[1..-1]]
      when '+'
        [[:plus], s[1..-1]]
      when '*'
        [[:star], s[1..-1]]
      when /[\(\)]/ # '(' or ')'
        [[m.to_sym], s[1..-1]]
      else
        error("terminal", "unrecognized terminal: #{s.inspect}")
        raise "Syntax Error, unrecognized terminal: #{s.inspect}"
      end
    end
  end
end