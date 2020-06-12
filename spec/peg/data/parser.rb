# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf/rule'
require 'ebnf/peg/parser'
require_relative 'terminals'
require 'sxp'

class EBNFPegParser
  include EBNF::PEG::Parser
  include EBNFPegParserTerminals

  # Abstract syntax tree from parse
  #
  # @return [Array<EBNF::Rule>]
  attr_reader :ast

  # Grammar errors, or errors found genering parse tables
  #
  # @return [Array<String>]
  attr_accessor :errors

  # ## Terminals
  # Define rules for Terminals, placing results on the input stack, making them available to upstream non-Terminal rules.
  #
  # Terminals are matched in the order of appearance

  # Match the Left hand side of a rule or terminal
  #
  #     [11] LHS        ::= ENUM? SYMBOL "::="
  terminal(:LHS, LHS) do |prod, value|
    # [id symbol]
    value.to_s.scan(/\[([^\]]+)\]\s*(\w+)\s*::=/).first
  end

  # Match `SYMBOL` terminal
  #
  #     [12] SYMBOL     ::= ([a-z] | [A-Z] | [0-9] | "_" | ".")+
  terminal(:SYMBOL, SYMBOL) do |prod, value|
    value.to_sym
  end

  # Match `HEX` terminal
  #
  #     [13] HEX        ::= '#x' ([0-9]|[a-f]|[A-F])+
  terminal(:HEX, HEX) do |prod, value|
    value.to_sym
  end

  # Terminal for `RANGE` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [14] `RANGE`      ::= '[' CHAR '-' CHAR ']'
  terminal(:RANGE, RANGE, unescape: true) do |prod, value|
    [:range, value[1..-2]]
  end

  # Terminal for `ENUM` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [15] ENUM       ::= '[' CHAR+ ']'
  terminal(:ENUM, ENUM, unescape: true) do |prod, value|
    [:range, value[1..-2]]
  end

  # Terminal for `O_RANGE` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [16] O_RANGE    ::= '[^' CHAR '-' CHAR ']'
  terminal(:O_RANGE, O_RANGE, unescape: true) do |prod, value|
    [:range, value[1..-2]]
  end

  # Terminal for `O_ENUM` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [17] O_ENUM     ::= '[^' CHAR+ ']'
  terminal(:O_ENUM, O_ENUM, unescape: true) do |prod, value|
    [:range, value[1..-2]]
  end

  # Strings have internal escape sequences expanded and are passed through without surrounding quotes as terminals

  # Match double quote string
  #
  #     [18] STRING1    ::= '"' (CHAR | [\t\'\[\]\(\)\-])* '"'
  terminal(:STRING1, STRING1, unescape: true) do |prod, value|
    value[1..-2]
  end

  # Match single quote string
  #
  #     [19] STRING2    ::= "'" (CHAR | [\t\"\[\]\(\)\-])* "'"
  terminal(:STRING2, STRING2, unescape: true) do |prod, value|
    value[1..-2]
  end

  # Match `POSTFIX` terminal
  #
  #     [21] POSTFIX    ::= [?*+]
  terminal(:POSTFIX, POSTFIX) do |prod, value|
    value
  end

  # ## Non-terminal productions
  # Define productions for non-Termainals. This can include `start_production` as well as `production` to hook into rule start and end. In some cases, we need to use sub-productions as generated when turning EBNF into BNF.

  # Production for end of `declaration` non-terminal.
  #
  # Look for `@terminals` to change parser state to parsing terminals.
  #
  # `@pass` is ignored here.
  #
  #     [2] declaration ::= '@terminals' | pass
  production(:declaration) do |data, value, callback|
    # current contains a declaration.
    # Invoke callback
    callback.call(:terminal) if value == '@terminals'
  end

  # Production for end of `rule` non-terminal.
  # The `input` parameter includes information placed by previous productions at the same level, or at the start of the current production.
  # The `current` parameter, is the result of child productions placing information onto their input.
  # The `callback` parameter provides access to a callback defined in the call to `parse`, see `#each_rule` below).
  #
  # Create rule from expression value and pass to callback
  #
  #     [3] rule        ::= LHS expression
  production(:rule) do |data, value, callback|
    # current contains an expression.
    # Invoke callback
    id, sym = value.first[:LHS]
    expression = value.last[:expression]
    callback.call(:rule, EBNF::Rule.new(sym.to_sym, id, expression))
  end

  # Production for end of `expression` non-terminal.
  # Passes through the optimized value of the alt production as follows:
  #
  #     [:alt foo] => foo
  #     [:alt foo bar] => [:alt foo bar]
  #
  #     [4] expression  ::= alt
  production(:expression) do |data, value, callback|
    value.first[:alt]
  end

  # Production for end of `alt` non-terminal.
  # Passes through the optimized value of the seq production as follows:
  #
  #     [:seq foo] => foo
  #     [:seq foo bar] => [:seq foo bar]
  #
  # Note that this also may just pass through from `_alt_1`
  #
  #     [5] alt         ::= seq ('|' seq)*
  #     [5] alt         ::= seq _alt_1
  production(:alt) do |data, value, callback|
    if value.last[:_alt_1].length > 0
      [:alt, value.first[:seq]] + value.last[:_alt_1]
    else
      value.first[:seq]
    end
  end

  # Production for end of `alt` non-terminal.
  # Passes through the optimized value of the seq production as follows:
  #
  #     [:seq foo] => foo
  #     [:seq foo bar] => [:seq foo bar]
  #
  # Note that this also may just pass through from `_alt_1`
  #
  #     [5] _alt_1         ::= ('|' seq)*
  production(:_alt_1) do |data, value, callback|
    value.map {|a1| a1.last[:seq]}.compact # Get rid of '|'
  end

  # Production for end of `seq` non-terminal.
  # Passes through the optimized value of the `diff` production as follows:
  #
  #     [:diff foo] => foo
  #     [:diff foo bar] => [:diff foo bar]
  #
  # Note that this also may just pass through from `_seq_1`
  #
  #     [6] seq         ::= diff+
  production(:seq) do |data, value, callback|
    case value.length
    when 0 then nil
    when 1 then value.first
    else        [:seq] + value
    end
  end

  # `Diff` production returns concatenated postfix values
  #
  #     [7] diff        ::= postfix ('-' postfix)?
  production(:diff) do |data, value, callback|
    if value.last[:_diff_1]
      [:diff, value.first[:postfix], value.last[:_diff_1]]
    else
      value.first[:postfix]
    end
  end

  production(:_diff_1) do |data, value, callback|
    value.last[:postfix] if value
  end

  # Production for end of `postfix` non-terminal.
  # Either returns the `primary` production value, or as modified by the `postfix`.
  #
  #     [8] postfix     ::= primary POSTFIX?
  production(:postfix) do |data, value, callback|
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
  production(:primary) do |data, value, callback|
    Array(value).length > 2 ? value[1][:expression] : value
  end

  # Production for end of pass non-terminal.
  #
  #     [10] pass       ::= '@pass' expression
  production(:pass) do |data, value, callback|
    # Invoke callback
    callback.call(:pass, value.last[:expression])
  end

  # ## Parser invocation.
  # On start, yield ourselves if a block is given, otherwise, return this parser instance
  #
  # @param  [#read, #to_s]          input
  # @param  [Hash{Symbol => Object}] options
  # @option options [Boolean] :progress
  #   Show progress of parser productions
  # @return [EBNFParser]
  def initialize(input, **options, &block)

    # Intantiate grammar from ebnf.ebnf
    ebnf = File.expand_path("../../../../etc/ebnf.ebnf", __FILE__)
    grammar = EBNF.parse(File.open(ebnf))
    rules = grammar.make_peg.ast

    @options = options.dup
    @input = input.respond_to?(:read) ? input.read : input.to_s

    parsing_terminals = false
    @ast = []
    parse(@input, :ebnf, rules,
                         whitespace: EBNFPegParserTerminals::PASS,
                         **options
    ) do |context, *data|
      rule = case context
      when :terminal
        parsing_terminals = true
        next
      when :pass
        rule = EBNF::Rule.new(nil, nil, data.first, kind: :pass)
      when :rule
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
    SXP::Generator.string(@ast.sort_by{|r| r.id.to_f}.map(&:for_sxp))
  end
end
