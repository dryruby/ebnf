# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf/rule'
require 'ebnf/terminals'
require 'ebnf/ll1/parser'
require_relative 'meta'
require 'sxp'

class EBNFParser
  include EBNF::LL1::Parser
  include EBNFParserMeta
  include EBNF::Terminals

  # Abstract syntax tree from parse
  #
  # @return [Array<EBNF::Rule>]
  attr_reader :ast

  terminal(:LHS, LHS) do |prod, token, input|
    input[:id], input[:symbol] = token.value.to_s.scan(/\[([^\]]+)\]\s*(\w+)\s*::=/).first
  end

  terminal(:SYMBOL, SYMBOL) do |prod, token, input|
    input[:terminal] = token.value.to_sym
  end

  terminal(:HEX, HEX) do |prod, token, input|
    input[:terminal] = token.value
  end

  terminal(:ENUM, ENUM, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  terminal(:O_ENUM, O_ENUM, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  terminal(:RANGE, RANGE, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  terminal(:O_RANGE, O_RANGE, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  terminal(:STRING1, STRING1, unescape: true) do |prod, token, input|
    input[:terminal] = token.value[1..-2]
  end

  terminal(:STRING2, STRING2, unescape: true) do |prod, token, input|
    input[:terminal] = token.value[1..-2]
  end

  terminal(:POSTFIX, POSTFIX) do |prod, token, input|
    input[:postfix] = token.value
  end

  terminal(nil,                  %r(@terminals|@pass|[\[\]|\-\(\)])) do |prod, token, input|
    input[:terminal] = token.value
  end

  production(:ebnf) do |input, current, callback|
    # Cause method_missing to invoke something in our context
    to_sxp
  end

  production(:declaration) do |input, current, callback|
    # current contains a declaration.
    # Invoke callback
    callback.call(:terminals) if current[:terminal] == '@terminals'
  end

  production(:rule) do |input, current, callback|
    # current contains an expression.
    # Invoke callback
    callback.call(:rule, EBNF::Rule.new(current[:symbol].to_sym, current[:id], current[:expression].last))
  end

  production(:expression) do |input, current, callback|
    alt = current[:alt]
    (input[:expression] ||= [:expression]) << (alt.length > 2 ? alt : alt.last)
  end

  production(:alt) do |input, current, callback|
    input[:alt] = if current[:alt]
      current[:alt]
    elsif seq = current[:seq]
      [:alt] << (seq.length > 2 ? seq : seq.last)
    end
  end

  start_production(:_alt_1) do |input, current, callback|
    seq = Array(input[:seq])
    (input[:alt] = [:alt]) << (seq.length > 2 ? seq : seq.last)
    input.delete(:seq)
  end

  production(:_alt_1) do |input, current, callback|
    input[:alt] ||= [:alt]

    # Add optimized value of `seq,` if any
    if seq = current[:seq]
      input[:alt] << (seq.length == 2 ? seq.last : seq)
    end

    # Also recursive call to `_alt_1`
    input[:alt] += current[:alt][1..-1] if current[:alt]
  end

  production(:seq) do |input, current, callback|
    input[:seq] = if current[:seq]
      current[:seq]
    elsif diff = current[:diff]
      [:seq] << (diff.length > 2 ? diff : diff.last)
    end
  end

  start_production(:_seq_1) do |input, current, callback|
    diff = Array(input[:diff])
    (input[:seq] = [:seq]) << (diff.length > 2 ? diff : diff.last)
    input.delete(:diff)
  end

  production(:_seq_1) do |input, current, callback|
    input[:seq] ||= [:seq]

    # Add optimized value of `diff`, if any
    if diff = current[:diff]
      input[:seq] << (diff.length > 2 ? diff : diff.last)
    end

    # Also recursive call to `_seq_1`
    input[:seq] += current[:seq][1..-1] if current[:seq]
  end

  production(:diff) do |input, current, callback|
    input[:diff] = if current[:diff]
      current[:diff]
    elsif postfix = current[:postfix]
      [:diff] << postfix
    end
  end

  start_production(:_diff_1) do |input, current, callback|
    postfix = Array(input[:postfix])
    (input[:diff] = [:diff]) << (postfix.length > 2 ? postfix : postfix.last)
    input.delete(:postfix)
  end

  production(:_diff_1) do |input, current, callback|
    # Gratuitous call to exercise method
    add_prod_data(:_diff_1, "foo")
    input[:diff] ||= [:diff]

    # Add optimized value of `postfix`, if any
    input[:diff] << current[:postfix] if current[:postfix]
  end

  production(:postfix) do |input, current, callback|
    # Gratuitous call to exercise method
    add_prod_datum(:postfix, "foo")
    # Push result onto input stack, as the `diff` production can have some number of `postfix` values that are applied recursively
    input[:postfix] =  case current[:postfix]
    when "*" then [:star, current[:primary]]
    when "+" then [:plus, current[:primary]]
    when "?" then [:opt, current[:primary]]
    else current[:primary]
    end
  end

  production(:primary) do |input, current, callback|
    # Gratuitous call to exercise method
    add_prod_datum(:primary, ["foo"])
    input[:primary] = if current[:expression]
      v = current[:expression][1..-1]
      v = v.first if v.length == 1
    else
      current[:terminal]
    end
  end

  production(:pass) do |input, current, callback|
    # Invoke callback
    callback.call(:pass, current[:expression].last)
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
                                whitespace: EBNF::Terminals::PASS,
                                reset_on_true: true,
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
  def to_sxp
    require 'sxp' unless defined?(SXP)
    # Output rules as a formatted S-Expression
    SXP::Generator.string(@ast.map(&:for_sxp))
  end
end
