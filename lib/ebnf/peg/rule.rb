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
    # * `alt`: returns the value of the matched production or `:unmatched`.
    # * `diff`: returns the string value matched, or `:unmatched`.
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
          result = (matched ? parser.onTerminal(sym, matched) : :unmatched)
          # Update furthest failure for strings and terminals
          parser.update_furthest_failure(input.pos, input.lineno, sym) if result == :unmatched
          parser.packrat[sym][pos] = {
            pos: input.pos,
            lineno: input.lineno,
            result: result
          }
          return parser.packrat[sym][pos][:result]
        end
      else
        eat_whitespace(input)
      end
      parser.onStart(sym)

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
          if alt == :unmatched
            # Update furthest failure for strings and terminals
            parser.update_furthest_failure(input.pos, input.lineno, prod) if prod.is_a?(String) || rule.terminal?
          else
            break
          end
        end
        alt
      when :diff
        # matches any string that matches A but does not match B.
        # (Note, this is only used for Terminal rules, non-terminals will use :not)
        raise "Diff used on non-terminal #{prod}" unless terminal?
        re1, re2 = Regexp.new(translate_codepoints(expr[1])), Regexp.new(translate_codepoints(expr[2]))
        matched = input.scan(re1)
        if !matched || re2.match?(matched)
          # Update furthest failure for terminals
          parser.update_furthest_failure(input.pos, input.lineno, sym)
          :unmatched
        else
          matched
        end
      when :hex
        # Matches the given hex character if expression matches the character whose number (code point) in ISO/IEC 10646 is N. The number of leading zeros in the #xN form is insignificant.
        input.scan(to_regexp) || begin
          # Update furthest failure for terminals
          parser.update_furthest_failure(input.pos, input.lineno, expr.last)
          :unmatched
        end
      when :not
        # matches any string that does not match B.
        res = case prod = expr[1]
        when Symbol
          rule = parser.find_rule(prod)
          raise "No rule found for #{prod}" unless rule
          rule.parse(input)
        when String
          input.scan(Regexp.new(Regexp.quote(prod))) || :unmatched
        end
        if res != :unmatched
          # Update furthest failure for terminals
          parser.update_furthest_failure(input.pos, input.lineno, sym) if terminal?
          :unmatched
        else
          nil
        end
      when :opt
        # Result is the matched value or nil
        opt = rept(input, 0, 1, expr[1])

        # Update furthest failure for strings and terminals
        parser.update_furthest_failure(input.pos, input.lineno, expr[1]) if terminal?
        opt.first
      when :plus
        # Result is an array of all expressions while they match,
        # at least one must match
        plus = rept(input, 1, '*', expr[1])

        # Update furthest failure for strings and terminals
        parser.update_furthest_failure(input.pos, input.lineno, expr[1]) if terminal?
        plus.is_a?(Array) && terminal? ? plus.join("") : plus
      when :range
        # Matches the specified character range
        input.scan(to_regexp) || begin
          # Update furthest failure for strings and terminals
          parser.update_furthest_failure(input.pos, input.lineno, expr[1])
          :unmatched
        end
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
          if res == :unmatched
            # Update furthest failure for strings and terminals
            parser.update_furthest_failure(input.pos, input.lineno, prod)
            break :unmatched 
          end
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
        star = rept(input, 0, '*', expr[1])

        # Update furthest failure for strings and terminals
        parser.update_furthest_failure(input.pos, input.lineno, expr[1]) if terminal?
        star.is_a?(Array) && terminal? ? star.join("") : star
      else
        raise "attempt to parse unknown rule type: #{expr.first}"
      end

      if result == :unmatched
        input.pos, input.lineno = pos, lineno
      end

      result = parser.onFinish(result)
      (parser.packrat[sym] ||= {})[pos] = {
        pos: input.pos,
        lineno: input.lineno,
        result: result
      }
      return parser.packrat[sym][pos][:result]
    end

    ##
    # Repitition, 0-1, 0-n, 1-n, ...
    #
    # Note, nil results are removed from the result, but count towards min/max calculations
    #
    # @param [Scanner] input
    # @param [Integer] min
    # @param [Integer] max
    #   If it is an integer, it stops matching after max entries.
    # @param [Symbol, String] prod
    # @return [:unmatched, Array]
    def rept(input, min, max, prod)
      result = []

      case prod
      when Symbol
        rule = parser.find_rule(prod)
        raise "No rule found for #{prod}" unless rule
        while (res = rule.parse(input)) != :unmatched && (max == '*' || result.length < max)
          eat_whitespace(input)
          result << res
        end
      when String
        while (res = input.scan(Regexp.new(Regexp.quote(prod)))) && (max == '*' || result.length < max)
          eat_whitespace(input)
          result << res
        end
      end

      result.length < min ? :unmatched : result.compact
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
