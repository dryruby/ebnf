module EBNF::PEG
  # Behaviior for parsing a PEG rule
  module Rule
    ##
    # Initialized by parser when loading rules.
    # Used for finding rules and invoking elements of the parse process.
    #
    # @return [EBNF::PEG::Parser] parser
    attr_accessor :parser

    ##
    # Parse a rule or terminal, invoking callbacks, as appropriate

    # If there is are `start_production` and/or `production`,
    # they are invoked with a `prod_data` stack, the input stream and offset.
    # Otherwise, the results are added as an array value
    # to a hash indexed by the rule name.
    #
    # If matched, the input position is updated and the results returned in a Hash.
    #
    # * `alt`: returns the value of the matched production or `:unmatched`
    # * `diff`: returns the string value matched, or `:unmatched`
    # * `hex`: returns a string composed of the matched hex character, or `:unmatched`.
    # * `opt`: returns the matched production, or `nil` if unmatched.
    # * `plus`: returns an array of the matches for the specified production, or `:unmatched`, if none are matched. For Terminals, these are concatenated into a single string.
    # * `range`: returns a string composed of the character matching the range, or `:unmatched`.
    # * `seq`: returns an array composed of single-entry hashes for each matched production indexed by the production name, or `:unmatched` if any production fails to match. For Terminals, returns a string created by concatenating these values.
    # * `star`: returns an array of the matches for the specified production.For Terminals, these are concatenated into a single string.
    # @param [Scanner] input
    # @return [Hash{Symbol => Object}, :unmatched] A hash with keys for matched component of the expression. Returns :unmatched if the input does not match the production.
    def parse(input)
      # Save position and linenumber for backtracking
      pos, lineno = input.pos, input.lineno

      parser.packrat[sym] ||= {}
      if parser.packrat[sym][pos]
        parser.debug("#{sym}(:memo)", lineno: lineno) { "#{parser.packrat[sym][pos].inspect}(@#{pos})"}
        input.pos, input.lineno = parser.packrat[sym][pos][:pos], parser.packrat[sym][pos][:lineno]
        return parser.packrat[sym][pos][:result]
      end

      if terminal?
        # If the terminal is defined with a regular expression,
        # use that to match the input,
        # otherwise,
        if regexp = parser.find_terminal_regexp(sym)
          matched = input.scan(regexp)
          parser.packrat[sym][pos] = {
            pos: input.pos,
            lineno: input.lineno,
            result: (matched ? parser.onTerminal(sym, matched, scanner: input) : :unmatched)
          }
          return parser.packrat[sym][pos][:result]
        end
      else
        eat_whitespace(input)
      end
      parser.onStart(sym, scanner: input)

      result = case expr.first
      when :alt
        # Return the first expression to match.
        # Result is either :unmatched, or the value of the matching rule
        alt = :unmatched
        expr[1..-1].each do |prod|
          alt = case prod
          when Symbol
            rule = parser.find_rule(prod)
            raise "No rule found for #{prod}" unless rule
            rule.parse(input)
          when String
            input.scan(Regexp.new(Regexp.quote(prod))) || :unmatched
          end
          break unless alt == :unmatched
        end
        alt
      when :diff
        # matches any string that matches A but does not match B.
        re1, re2 = Regexp.new(translate_codepoints(expr[1])), Regexp.new(translate_codepoints(expr[2]))
        matched = input.scan(re1)
        !matched || re2.match?(matched) ? :unmatched : matched
      when :hex
        # Matches the given hex character if expression matches the character whose number (code point) in ISO/IEC 10646 is N. The number of leading zeros in the #xN form is insignificant.
        input.scan(to_regexp) || :unmatched
      when :opt
        # Always matches
        opt = case prod = expr[1]
        when Symbol
          rule = parser.find_rule(prod)
          raise "No rule found for #{prod}" unless rule
          rule.parse(input)
        when String
          input.scan(Regexp.new(Regexp.quote(prod))) || :unmatched
        end
        opt == :unmatched ? nil : opt
      when :plus
        # Result is an array of all expressions while they match,
        # at least one must match
        prod, plus = expr[1], []
        case prod
        when Symbol
          rule = parser.find_rule(prod)
          raise "No rule found for #{prod}" unless rule
          while (res = rule.parse(input)) != :unmatched
            eat_whitespace(input)
            plus << res
          end
        when String
          while res = input.scan(Regexp.new(Regexp.quote(prod)))
            eat_whitespace(input)
            plus << res
          end
        end
        plus.empty? ? :unmatched : (terminal? ? plus.compact.join("") : plus.compact)
      when :range
        # Matches the specified character range
        input.scan(to_regexp) || :unmatched
      when :seq
        # Evaluate each expression into an array of hashes where each hash contains a key from the associated production and the value is the parsed value of that production. Returns :unmatched if the input does not match the production. Value ordering is ensured by native Hash ordering.
        seq = expr[1..-1].each_with_object([]) do |prod, accumulator|
          eat_whitespace(input) unless accumulator.empty?
          res = case prod
          when Symbol
            rule = parser.find_rule(prod)
            raise "No rule found for #{prod}" unless rule
            rule.parse(input)
          when String
            input.scan(Regexp.new(Regexp.quote(prod))) || :unmatched
          end
          break :unmatched if res == :unmatched
          accumulator << {prod.to_sym => res}
        end
        seq == :unmatched ?
          :unmatched :
          (terminal? ?
            seq.map(&:values).compact.join("") : # Concat values for terminal production
            seq)
      when :star
        # Result is an array of all expressions while they match,
        # an empty array of none match
        prod, star = expr[1], []
        case prod
        when Symbol
          rule = parser.find_rule(prod)
          raise "No rule found for #{prod}" unless rule
          while (res = rule.parse(input)) != :unmatched
            eat_whitespace(input)
            star << res
          end
        when String
          while res = input.scan(Regexp.new(Regexp.quote(prod)))
            eat_whitespace(input)
            star << res
          end
        end
        star.compact
      else
        raise "attempt to parse unknown rule type: #{expr.first}"
      end

      if result == :unmatched
        input.pos, input.lineno = pos, lineno
      end

      result = parser.onFinish(result, scanner: input)
      (parser.packrat[sym] ||= {})[pos] = {
        pos: input.pos,
        lineno: input.lineno,
        result: result
      }
      return parser.packrat[sym][pos][:result]
    end

    ##
    # Eat whitespace between non-terminal rules
    def eat_whitespace(input)
      if parser.whitespace.is_a?(Regexp)
        # Eat whitespace before a non-terminal
        input.skip(parser.whitespace)
      elsif parser.whitespace.is_a?(Rule)
        parser.whitespace.parse(input) # throw away result
      end
    end
  end
end
