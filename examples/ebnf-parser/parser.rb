# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf/rule'
require 'ebnf/ll1/parser'
require 'meta'
require 'terminals'
require 'sxp'

class EBNFParser
  include EBNF::LL1::Parser
  include EBNFParserMeta
  include EBNFParserTerminals

  class ProdResult
    attr_accessor :prod
    attr_accessor :values

    def initialize(prod, *values)
      @prod, @values = prod, values
    end

    def to_ary
      values.map {|v| v.respond_to?(:to_ary) ? v.to_ary : v}.unshift(@prod)
    end

    def inspect
      "(#{prod} #{values.map(&:inspect).join(' ')})"
    end
  end

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
  terminal(:LHS, LHS) do |prod, token, input|
    input[:id], input[:symbol] = token.value.to_s.scan(/\[([^\]]+)\]\s*(\w+)\s*::=/).first
  end

  # Match `SYMBOL` terminal
  #
  #     [12] SYMBOL     ::= ([a-z] | [A-Z] | [0-9] | "_" | ".")+
  terminal(:SYMBOL, SYMBOL) do |prod, token, input|
    input[:terminal] = token.value.to_sym
  end

  # Terminal for `RANGE` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [14] `RANGE`      ::= '[' CHAR '-' CHAR ']'
  terminal(:RANGE, RANGE, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Terminal for `ENUM` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [15] ENUM       ::= '[' CHAR+ ']'
  terminal(:ENUM, ENUM, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Terminal for `O_RANGE` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [16] O_RANGE    ::= '[^' CHAR '-' CHAR ']'
  terminal(:O_RANGE, O_RANGE, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Terminal for `O_ENUM` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [17] O_ENUM     ::= '[^' CHAR+ ']'
  terminal(:O_ENUM, O_ENUM, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Strings have internal escape sequences expanded and are passed through without surrounding quotes as terminals

  # Match double quote string
  #
  #     [18] STRING1    ::= '"' (CHAR | [\t\'\[\]\(\)\-])* '"'
  terminal(:STRING1, STRING1, unescape: true) do |prod, token, input|
    input[:terminal] = token.value[1..-2]
  end

  # Match single quote string
  #
  #     [19] STRING2    ::= "'" (CHAR | [\t\"\[\]\(\)\-])* "'"
  terminal(:STRING2, STRING2, unescape: true) do |prod, token, input|
    input[:terminal] = token.value[1..-2]
  end

  # Match `POSTFIX` terminal
  #
  #     [21] POSTFIX    ::= [?*+]
  terminal(:POSTFIX, POSTFIX) do |prod, token, input|
    input[:postfix] = token.value
  end

  # Make sure we recognize string terminals, even though they're not actually used in processing
  terminal(nil,                  %r(@terminals|@pass|[\[\]|\-\(\)])) do |prod, token, input|
    input[:terminal] = token.value
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
  production(:declaration) do |input, data, callback|
    # data contains a declaration.
    # Invoke callback
    if data[:terminal]
      callback.call(:terminal, data[:terminal])
    elsif data[:pass]
      callback.call(:pass, data[:pass])
    end
  end

  # Production for end of `rule` non-terminal.
  # The `input` parameter includes information placed by previous productions at the same level, or at the start of the current production.
  # The `data` parameter, is the result of child productions placing information onto their input.
  # The `callback` parameter provides access to a callback defined in the call to `parse`, see `#each_rule` below).
  #
  # Create rule from expression value and pass to callback
  #
  #     [3] rule        ::= LHS expression
  production(:rule) do |input, data, callback|
    # data contains an expression.
    # Invoke callback
    expr = data[:expression].respond_to?(:to_ary) ? data[:expression].to_ary : data[:expression]
    callback.call(:rule, EBNF::Rule.new(data[:symbol].to_sym, data[:id], expr))
  end

  # Production for end of `expression` non-terminal.
  # Passes through the optimized value of the alt production as follows:
  #
  #     [:alt foo] => foo
  #     [:alt foo bar] => [:alt foo bar]
  #
  #     [4] expression  ::= alt
  production(:expression) do |input, data, callback|
    input[:expression] = data[:alt]
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
  production(:alt) do |input, data, callback|
    input[:alt] = if data[:seq].length > 1
      ProdResult.new(:alt, *data[:seq])
    else
      data[:seq].first
    end
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
  production(:seq) do |input, data, callback|
    input[:seq] ||= []
    input[:seq] << if data[:diff].length > 1
      ProdResult.new(:seq, *data[:diff])
    else
      data[:diff].first
    end
  end

  # `Diff` production returns concatenated postfix values
  #
  #     [7] diff        ::= postfix ('-' postfix)?
  production(:diff) do |input, data, callback|
    input[:diff] ||= []
    input[:diff] << if data[:postfix].length > 1
      ProdResult.new(:diff, *data[:postfix])
    else
      data[:postfix].first
    end
  end

  # Production for end of `postfix` non-terminal.
  # Either returns the `primary` production value, or as modified by the `postfix`.
  #
  #     [:primary] => [:primary]
  #     [:primary, '*'] => [:star, :primary]
  #     [:primary, '+'] => [:plus, :primary]
  #     [:primary, '?'] => [:opt, :primary]
  #
  #     [8] postfix     ::= primary POSTFIX?
  production(:postfix) do |input, data, callback|
    # Push result onto input stack, as the `diff` production can have some number of `postfix` values that are applied recursively
    input[:postfix] ||= []
    input[:postfix] <<  case data[:postfix]
    when "*" then ProdResult.new(:star, data[:primary])
    when "+" then ProdResult.new(:plus, data[:primary])
    when "?" then ProdResult.new(:opt, data[:primary])
    else data[:primary]
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
  production(:primary) do |input, data, callback|
    input[:primary] = data[:expression] || data[:terminal]
  end

  # Production for end of pass non-terminal.
  #
  #     [10] pass       ::= '@pass' expression
  production(:pass) do |input, data, callback|
    input[:pass] = data[:expression]
  end

  # ## Parser invocation.
  # On start, yield ourselves if a block is given, otherwise, return this parser instance
  #
  # @param  [#read, #to_s]          input
  # @param  [Hash{Symbol => Object}] options
  # @option options [Hash]     :prefixes     (Hash.new)
  #   the prefix mappings to use (for acessing intermediate parser productions)
  # @option options [Boolean] :progress
  #   Show progress of parser productions
  # @return [EBNFParser]
  def initialize(input, **options, &block)
    @options = options.dup
    @input = input.respond_to?(:read) ? input.read : input.to_s

    parsing_terminals = false
    @ast = []
    parse(@input, START.to_sym, branch: BRANCH,
                                first: FIRST,
                                follow: FOLLOW,
                                cleanup: CLEANUP,
                                whitespace: EBNFParserTerminals::PASS,
                                reset_on_start: true,
                                **options)
    ) do |context, *data|
      rule = case context
      when :terminal
        parsing_terminals = true
        rule = EBNF::Rule.new(nil, nil, data.first, kind: :terminal)
      when :pass
        rule = EBNF::Rule.new(nil, nil, data.first, kind: :pass)
      when :rule
        rule = data.first
        rule.kind = :terminal if parsing_terminals
        rule
      when :trace
        level, lineno, depth, *args = data
        message = "#{args.join(': ')}"
        d_str = depth > 100 ? ' ' * 100 + '+' : ' ' * depth
        $stderr.puts "[#{lineno}](#{level})#{d_str}#{message}" if @options[:progress] || @options[:debug] == true
        next
      end
      @ast << rule
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
