# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf/rule'
require 'ebnf/terminals'
require 'ebnf/ll1/parser'
require 'meta'
require 'sxp'
require 'logger'

class EBNFLL1Parser
  include EBNF::LL1::Parser
  include EBNFParserMeta
  include EBNF::Terminals

  # An internal class used for capturing the values of a production.
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

  # ## Terminals
  # Define rules for Terminals, placing results on the input stack, making them available to upstream non-Terminal rules.
  #
  # Terminals are defined with a symbol matching the associated rule name, and a regular expression used by the lexer.
  #
  # The `prod` parameter is the name of the parent rule for which this terminal is matched, which may have a bearing in some circumstances, although not used in this example.
  #
  # The `token` parameter is the matched lexer token.
  #
  # The `input` is used for returning the semantic value(s) of this terminal, which if often a string, but may be any instance which reflects the semantic interpretation of that terminal.
  #
  # Terminals are matched in the order of appearance

  # Match the Left hand side of a rule or terminal
  #
  #     [11] LHS        ::= ('[' SYMBOL+ ']' ' '+)? SYMBOL ' '* '::='
  terminal(:LHS, LHS) do |prod, token, input|
    input[:id], input[:symbol] = token.value.to_s.scan(/(?:\[([^\]]+)\])?\s*(\w+)\s*::=/).first
  end

  # Match `SYMBOL` terminal
  #
  #     [12] SYMBOL     ::= ([a-z] | [A-Z] | [0-9] | '_' | '.')+
  terminal(:SYMBOL, SYMBOL) do |prod, token, input|
    input[:terminal] = token.value.to_sym
  end

  # Match `HEX` terminal
  #
  #     [13] HEX        ::= '#x' ([a-f] | [A-F] | [0-9])+
  terminal(:HEX, HEX) do |prod, token, input|
    input[:terminal] = [:hex, token.value]
  end

  # Terminal for `RANGE` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [14] `RANGE`      ::= '[' (R_CHAR '-' R_CHAR) | (HEX '-' HEX) ']'
  terminal(:RANGE, RANGE, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Terminal for `O_RANGE` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [15] O_RANGE    ::= '[^' (R_CHAR '-' R_CHAR) | (HEX '-' HEX) ']'
  terminal(:O_RANGE, O_RANGE, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Strings have internal escape sequences expanded and are passed through without surrounding quotes as terminals

  # Match double quote string
  #
  #     [16] STRING1    ::= '"' (CHAR - '"')* '"'
  terminal(:STRING1, STRING1, unescape: true) do |prod, token, input|
    input[:terminal] = token.value[1..-2].tap {|s| s.quote_style = :dquote}
  end

  # Match single quote string
  #
  #     [17] STRING2    ::= "'" (CHAR - "'")* "'"
  terminal(:STRING2, STRING2, unescape: true) do |prod, token, input|
    input[:terminal] = token.value[1..-2].tap {|s| s.quote_style = :squote}
  end

  # The `CHAR` and `R_CHAR` productions are not used explicitly

  # Match `POSTFIX` terminal
  #
  #     [20] POSTFIX    ::= [?*+]
  terminal(:POSTFIX, POSTFIX) do |prod, token, input|
    input[:postfix] = token.value
  end

  # The `PASS` productions is not used explicitly

  # Make sure we recognize string terminals, even though they're not actually used in processing. This defines a "catch-all" terminal for the lexer.
  terminal(nil,                  %r(@terminals|@pass|[\[\]|\-\(\)])) do |prod, token, input|
    input[:terminal] = token.value
  end

  # ## Non-terminal productions
  # Define productions for non-Termainals. This can include `start_production` as well as `production` to hook into rule start and end. In some cases, we need to use sub-productions as generated when turning EBNF into BNF.
  #
  # The `input` parameter is a Hash containing input from the parent production. and is  used for returning the results of this production.
  #
  # The `data` parameter data returned by child productions placing information onto their input.
  #
  # The `callback` parameter provides access to a callback defined in the call to `parse`, see `#each_rule` below).

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
      callback.call(:terminals, data[:terminal])
    elsif data[:pass]
      callback.call(:pass, data[:pass])
    end
  end

  # Production for end of `rule` non-terminal.
  # The `input` parameter includes information placed by previous productions at the same level, or at the start of the current production.
  # The `data` parameter, is the result of child productions placing information onto their input.
  # The `callback` parameter provides access to a callback defined in the call.
  #
  # Create rule from expression value and pass to callback
  #
  #     [3] rule        ::= LHS expression
  production(:rule) do |input, data, callback|
    # data contains an expression.
    # Invoke callback
    expr = data[:expression].respond_to?(:to_ary) ? data[:expression].to_ary : data[:expression]
    callback.call(:rule, EBNF::Rule.new(data[:symbol].to_sym, data[:id], expr)) if expr
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
    data[:postfix] ||= []
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
    expression = data[:expression]
    expression = expression.to_ary if expression.respond_to?(:to_ary)
    input[:pass] = expression
  end

  # ## Parser invocation.
  # On start, yield ourselves if a block is given, otherwise, return this parser instance
  #
  # @param  [#read, #to_s]          input
  # @param  [Hash{Symbol => Object}] options
  # @option options [Boolean] :level
  #   Trace level. 0(debug), 1(info), 2(warn), 3(error).
  # @return [self]
  def initialize(input, **options, &block)
    # Read input, if necessary, which will be used in a Scanner which feads the Lexer.
    @input = input.respond_to?(:read) ? input.read : input.to_s

    # If the `level` option is set, instantiate a logger for collecting trace information.
    if options.key?(:level)
      options[:logger] ||= Logger.new(STDERR).
        tap {|x| x.level = options[:level]}.
        tap {|x| x.formatter = lambda {|severity, datetime, progname, msg| "#{severity} #{msg}\n"}}
    end

    parsing_terminals = false
    @ast = []
    parse(@input, START.to_sym, branch: BRANCH,
                                first: FIRST,
                                follow: FOLLOW,
                                cleanup: CLEANUP,
                                whitespace: EBNF::Terminals::PASS,
                                reset_on_start: true,
                                **options
    ) do |context, *data|
      rule = case context
      when :terminals
        # After parsing `@terminals`
        # This changes the state of the parser to treat subsequent rules as terminals.
        parsing_terminals = true
        rule = EBNF::Rule.new(nil, nil, data.first, kind: :terminals)
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
  #
  # @return [String]
  def to_sxp(**options)
    require 'sxp' unless defined?(SXP)
    # Output rules as a formatted S-Expression
    SXP::Generator.string(@ast.map(&:for_sxp))
  end
end
