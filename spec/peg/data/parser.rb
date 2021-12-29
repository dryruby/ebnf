# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf/rule'
require 'ebnf/terminals'
require 'ebnf/peg/parser'
require 'sxp'

class EBNFPegParser
  include EBNF::PEG::Parser
  include EBNF::Terminals

  # Abstract syntax tree from parse
  #
  # @return [Array<EBNF::Rule>]
  attr_reader :ast

  terminal(:LHS, LHS) do |value|
    # [id symbol]
    value.to_s.scan(/\[([^\]]+)\]\s*(\w+)\s*::=/).first
  end

  terminal(:SYMBOL, SYMBOL) do |value|
    value.to_sym
  end

  terminal(:HEX, HEX)

  terminal(:RANGE, RANGE, unescape: true) do |value|
    [:range, value[1..-2]]
  end

  terminal(:O_RANGE, O_RANGE, unescape: true) do |value|
    [:range, value[1..-2]]
  end

  terminal(:STRING1, STRING1, unescape: true) do |value|
    value[1..-2]
  end

  terminal(:STRING2, STRING2, unescape: true) do |value|
    value[1..-2]
  end

  terminal(:POSTFIX, POSTFIX)

  production(:ebnf) do |input|
    # Cause method_missing to invoke something in our context
    to_sxp
  end

  production(:declaration, clear_packrat: true) do |value, data, callback|
    # current contains a declaration.
    # Invoke callback
    callback.call(:terminals) if value == '@terminals'
  end

  start_production(:rule, as_hash: true)
  production(:rule, clear_packrat: true) do |value, data, callback|
    # current contains an expression.
    # Invoke callback
    id, sym = value[:LHS]
    expression = value[:expression]
    callback.call(:rule, EBNF::Rule.new(sym.to_sym, id, expression))
  end

  production(:expression) do |value|
    value.first[:alt]
  end

  production(:alt) do |value|
    if value.last[:_alt_1].length > 0
      [:alt, value.first[:seq]] + value.last[:_alt_1]
    else
      value.first[:seq]
    end
  end

  production(:_alt_1) do |value|
    value.map {|a1| a1.last[:seq]}.compact # Get rid of '|'
  end

  production(:seq) do |value|
    value.length == 1 ? value.first : ([:seq] + value)
  end

  start_production(:diff, as_hash: true)
  production(:diff) do |value|
    if value[:_diff_1]
      [:diff, value[:postfix], value[:_diff_1]]
    else
      value[:postfix]
    end
  end

  production(:_diff_1) do |value|
    value.last[:postfix] if value
  end

  start_production(:postfix, as_hash: true)
  production(:postfix) do |value|
    # Push result onto input stack, as the `diff` production can have some number of `postfix` values that are applied recursively
    case value[:_postfix_1]
    when "*" then [:star, value[:primary]]
    when "+" then [:plus, value[:primary]]
    when "?" then [:opt, value[:primary]]
    else value[:primary]
    end
  end

  production(:primary) do |value|
    Array(value).length > 2 ? value[1][:expression] : value
  end

  start_production(:pass, as_hash: true)
  production(:pass) do |value, data, callback|
    # Invoke callback
    callback.call(:pass, value[:expression])
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
                         whitespace: EBNF::Terminals::PASS,
                         **options
    ) do |context, *data|
      rule = case context
      when :terminals
        parsing_terminals = true
        rule = EBNF::Rule.new(nil, nil, data.first, kind: :terminals)
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
  #
  # @return [String]
  def to_sxp(**options)
    require 'sxp' unless defined?(SXP)
    # Output rules as a formatted S-Expression
    SXP::Generator.string(@ast.map(&:for_sxp))
  end
end
