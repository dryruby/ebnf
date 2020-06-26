# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf'
require 'ebnf/terminals'
require 'ebnf/peg/parser'
require 'meta'
require 'sxp'
require 'logger'

class EBNFPegParser
  include EBNF::PEG::Parser
  include EBNF::Terminals

  # Abstract syntax tree from parse
  #
  # @return [Array<EBNF::Rule>]
  attr_reader :ast

  # ## Terminals
  # Define rules for Terminals, placing results on the input stack, making them available to upstream non-Terminal rules.
  #
  # Terminals are defined with a symbol matching the associated rule name, and an optional (although strongly encouraged) regular expression used to match the head of the input stream.
  #
  # The result of the terminal block is the semantic value of that terminal, which if often a string, but may be any instance which reflects the semantic interpretation of that terminal.
  #
  # The `value` parameter is the value matched by the regexp, if defined, or by the sub-terminal rules otherwise.
  #
  # The `prod` parameter is the name of the parent rule for which this terminal is matched, which may have a bearing in some circumstances, although not used in this example.
  #
  # If no block is provided, then the value which would have been passed to the block is used as the result directly.

  # Match the Left hand side of a rule or terminal
  #
  #     [11] LHS        ::= ('[' SYMBOL+ ']' ' '+)? SYMBOL ' '* '::='
  terminal(:LHS, LHS) do |value, prod|
    value.to_s.scan(/\[([^\]]+)\]\s*(\w+)\s*::=/).first
  end

  # Match `SYMBOL` terminal
  #
  #     [12] SYMBOL     ::= ([a-z] | [A-Z] | [0-9] | '_' | '.')+
  terminal(:SYMBOL, SYMBOL) do |value|
    value.to_sym
  end

  # Match `HEX` terminal
  #
  #     [13] HEX        ::= #x' ([a-f] | [A-F] | [0-9])+
  terminal(:HEX, HEX)

  # Terminal for `ENUM` is matched as part of a `primary` rule.
  #
  #     [14] ENUM       ::= ('[' R_CHAR+ | HEX+ ']') - LHS
  terminal(:ENUM, ENUM) do |value|
    [:range, value[1..-2]]
  end

  # Terminal for `O_ENUM` is matched as part of a `primary` rule.
  #
  #     [15] O_ENUM     ::= '[^' R_CHAR+ | HEX+ ']'
  terminal(:O_ENUM, O_ENUM) do |value|
    [:range, value[1..-2]]
  end

  # Terminal for `RANGE` is matched as part of a `primary` rule.
  #
  #     [16] `RANGE`      ::= '[' (R_CHAR '-' R_CHAR) | (HEX '-' HEX) ']'
  terminal(:RANGE, RANGE) do |value|
    [:range, value[1..-2]]
  end

  # Terminal for `O_RANGE` is matched as part of a `primary` rule.
  #
  #     [17] O_RANGE    ::= '[^' (R_CHAR '-' R_CHAR) | (HEX '-' HEX) ']'
  terminal(:O_RANGE, O_RANGE) do |value|
    [:range, value[1..-2]]
  end

  # Match double quote string
  #
  #     [18] STRING1    ::= '"' (CHAR - '"')* '"'
  terminal(:STRING1, STRING1) do |value|
    value[1..-2]
  end

  # Match single quote string
  #
  #     [19] STRING2    ::= "'" (CHAR - "'")* "'"
  terminal(:STRING2, STRING2) do |value|
    value[1..-2]
  end

  # The `CHAR` and `R_CHAR` productions are not used explicitly

  # Match `POSTFIX` terminal
  #
  #     [22] POSTFIX    ::= [?*+]
  terminal(:POSTFIX, POSTFIX)

  # The `PASS` productions is not used explicitly

  # ## Non-terminal productions
  # Define productions for non-Termainals. This can include `start_production` as well as `production` to hook into rule start and end. In some cases, we need to use sub-productions as generated when turning EBNF into PEG.
  #
  # Productions are defined with a symbol matching the associated rule name.
  #
  # The result of the productions is typically the abstract syntax tree matched by the rule, so far, but could be a specific semantic value, or could be ignored with the result being returned via the `callback`.
  #
  # The `value` parameter is the result returned from child productions
  #
  # The `data` parameter other data which may be returned by child productions placing information onto their input (unused in this example).
  #
  # The `callback` parameter provides access to a callback defined in the call to `parse`).

  # Production for end of `declaration` non-terminal.
  #
  # Look for `@terminals` to change parser state to parsing terminals.
  #
  # Clears the packrat parser when called.
  #
  # `@pass` is ignored here.
  #
  #     [2] declaration ::= '@terminals' | pass
  production(:declaration, clear_packrat: true) do |value, data, callback|
    # value contains a declaration.
    # Invoke callback
    callback.call(:terminal) if value == '@terminals'
    nil
  end

  # Production for end of `rule` non-terminal.
  #
  # The `value` parameter, is of the form `[{LHS: "v"}, {expression: "v"}]`.
  #
  # Clears the packrat parser when called.
  #
  # Create rule from expression value and pass to callback
  #
  #     [3] rule        ::= LHS expression
  production(:rule, clear_packrat: true) do |value, data, callback|
    # value contains an expression.
    # Invoke callback
    id, sym = value.first[:LHS]
    expression = value.last[:expression]
    callback.call(:rule, EBNF::Rule.new(sym.to_sym, id, expression))
    nil
  end

  # Production for end of `expression` non-terminal.
  # Passes through the optimized value of the alt production as follows:
  #
  # The `value` parameter, is of the form `[{alt: "v"}]`.
  #
  #     [:alt foo] => foo
  #     [:alt foo bar] => [:alt foo bar]
  #
  #     [4] expression  ::= alt
  production(:expression) do |value|
    value.first[:alt]
  end

  # Production for end of `alt` non-terminal.
  # Passes through the optimized value of the seq production as follows:
  #
  # The `value` parameter, is of the form `[{seq: "v"}, {_alt_1: "v"}]`.
  #
  #     [:seq foo] => foo
  #     [:seq foo bar] => [:seq foo bar]
  #
  # Note that this also may just pass through from `_alt_1`
  #
  #     [5] alt         ::= seq ('|' seq)*
  production(:alt) do |value|
    if value.last[:_alt_1].length > 0
      [:alt, value.first[:seq]] + value.last[:_alt_1]
    else
      value.first[:seq]
    end
  end

  # Production for end of `_alt_1` non-terminal.
  # Used to collect the `('|' seq)*` portion of the `alt` non-terminal:
  #
  # The `value` parameter, is of the form `[{seq: ["v"]}]`.
  #
  #     [5] _alt_1         ::= ('|' seq)*
  production(:_alt_1) do |value|
    value.map {|a1| a1.last[:seq]}.compact # Get rid of '|'
  end

  # Production for end of `seq` non-terminal.
  # Passes through the optimized value of the `diff` production as follows:
  #
  # The `value` parameter, is an array of values, which cannot be empty.
  #
  #     [:diff foo] => foo
  #     [:diff foo bar] => [:diff foo bar]
  #
  # Note that this also may just pass through from `_seq_1`
  #
  #     [6] seq         ::= diff+
  production(:seq) do |value|
    value.length == 1 ? value.first : ([:seq] + value)
  end

  # `Diff` production returns concatenated postfix values
  #
  # The `value` parameter, is of the form `[{postfix: "v"}, {_diff_1: "v"}]`.
  #
  #     [7] diff        ::= postfix ('-' postfix)?
  production(:diff) do |value|
    if value.last[:_diff_1]
      [:diff, value.first[:postfix], value.last[:_diff_1]]
    else
      value.first[:postfix]
    end
  end

  production(:_diff_1) do |value|
    value.last[:postfix] if value
  end

  # Production for end of `postfix` non-terminal.
  # Either returns the `primary` production value, or as modified by the `postfix`.
  #
  # The `value` parameter, is of the form `[{primary: "v"}, {_postfix_1: "v"}]`.
  #
  #     [:primary] => [:primary]
  #     [:primary, '*'] => [:star, :primary]
  #     [:primary, '+'] => [:plus, :primary]
  #     [:primary, '?'] => [:opt, :primary]
  #
  #     [8] postfix     ::= primary POSTFIX?
  production(:postfix) do |value|
    # Push result onto input stack, as the `diff` production can have some number of `postfix` values that are applied recursively
    case value.last[:_postfix_1]
    when "*" then [:star, value.first[:primary]]
    when "+" then [:plus, value.first[:primary]]
    when "?" then [:opt, value.first[:primary]]
    else value.first[:primary]
    end
  end

  # Production for end of `primary` non-terminal.
  # Places `:primary` on the stack
  #
  # The `value` parameter, is either a string (for a terminal) or an array of the form `['(': '(', expression: "v", ')', ')']`.
  #
  # This may either be a terminal, or the result of an `expression`.
  #
  #     [9] primary     ::= HEX
  #                     |   SYMBOL
  #                     |   RANGE
  #                     |   ENUM
  #                     |   O_RANGE
  #                     |   O_ENUM
  #                     |   STRING1
  #                     |   STRING2
  #                     |   '(' expression ')'
  production(:primary) do |value|
    Array(value).length > 2 ? value[1][:expression] : value
  end

  # Production for end of pass non-terminal.
  #
  #     [10] pass       ::= '@pass' expression
  production(:pass) do |value, data, callback|
    # Invoke callback
    callback.call(:pass, value.last[:expression])
  end

  # ## Parser invocation.
  # On start, yield ourselves if a block is given, otherwise, return this parser instance
  #
  # @param  [#read, #to_s]          input
  # @param  [Hash{Symbol => Object}] options
  # @option options [Boolean] :level
  #   Trace level. 0(debug), 1(info), 2(warn), 3(error).
  # @return [EBNFParser]
  def initialize(input, **options, &block)
    # If the `level` option is set, instantiate a logger for collecting trace information.
    if options.has_key?(:level)
      options[:logger] = Logger.new(STDERR)
      options[:logger].level = options[:level]
      options[:logger].formatter = lambda {|severity, datetime, progname, msg| "#{severity} #{msg}\n"}
    end

    # Read input, if necessary, which will be used in a Scanner.
    @input = input.respond_to?(:read) ? input.read : input.to_s

    parsing_terminals = false
    @ast = []
    parse(@input, :ebnf, EBNFPegMeta::RULES,
                         # Use an optimized Regexp for whitespace
                         whitespace: EBNF::Terminals::PASS,
                         **options
    ) do |context, *data|
      rule = case context
      when :terminal
        # After parsing `@terminals`
        # This changes the state of the parser to treat subsequent rules as terminals.
        parsing_terminals = true
        next
      when :pass
        # After parsing `@pass`
        # This defines a specific rule for whitespace.
        rule = EBNF::Rule.new(nil, nil, data.first, kind: :pass)
      when :rule
        # A rule which has already been turned into a `Rule` object.
        rule = data.first
        rule.kind = :terminal if parsing_terminals
        rule
      end
      @ast << rule if rule
    end
    @ast
  end

  # Output formatted S-Expression of grammar
  def to_sxp
    require 'sxp' unless defined?(SXP)
    # Output rules as a formatted S-Expression
    SXP::Generator.string(@ast.map(&:for_sxp))
  end
end
