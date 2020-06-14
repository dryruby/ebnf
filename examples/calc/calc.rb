# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf'
require 'ebnf/terminals'
require 'ebnf/peg/parser'
require 'sxp'
require 'logger'

class Calc
  include EBNF::PEG::Parser

  # Abstract syntax tree from parse
  #
  # @return [Array<EBNF::Rule>]
  attr_reader :ast

  ##
  # The calculator grammar comes from a [Wikipedia entry on Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples), with some small concessions.
  #
  #     [1] Expr    ::= Sum
  #     [2] Sum     ::= Product (('+' | '-') Product)*
  #     [3] Product ::= Power (('*' | '/') Power)*
  #     [4] Power   ::= Value ('^' Power)?
  #     [5] Value   ::= NUMBER | '(' Expr ')'
  #     [6] NUMBER  ::= [0-9]+
  #
  # This, in turn, is turned into S-Expression with sub-rules added for embedded rules, which allow them to be accessed independently:
  #
  #     (
  #      (rule Expr "1" (seq Sum))
  #      (rule Sum "2" (seq Product _Sum_1))
  #      (rule _Sum_1 "2.1" (star _Sum_2))
  #      (rule _Sum_2 "2.2" (seq _Sum_3 Product))
  #      (rule _Sum_3 "2.3" (alt "+" "-"))
  #      (rule Product "3" (seq Power _Product_1))
  #      (rule _Product_1 "3.1" (star _Product_2))
  #      (rule _Product_2 "3.2" (seq _Product_3 Power))
  #      (rule _Product_3 "3.3" (alt "*" "/"))
  #      (rule Power "4" (seq Value _Power_1))
  #      (rule _Power_1 "4.1" (opt _Power_2))
  #      (rule _Power_2 "4.2" (seq "^" Power))
  #      (rule Value "5" (alt NUMBER _Value_1))
  #      (rule _Value_1 "5.1" (seq "(" Expr ")"))
  #      (terminal NUMBER "6" (plus _NUMBER_1))
  #      (terminal _NUMBER_1 "6.1" (range "0-9")))

  ##
  # The calculator evaluates values from each rule and applies operators resulting in the calculated result.

  # [1] Expr    := Sum
  #
  #      (rule Expr "1" (seq Sum))
  production(:Expr, clear_packrat: true) do |value|
    value.first[:Sum]
  end

  # [2] Sum     := Product (('+' | '-') Product)\*
  #
  #      (rule Sum "2" (seq Product _Sum_1))
  #      (rule _Sum_1 "2.1" (star _Sum_2))
  production(:Sum, clear_packrat: true) do |value|
    product, operations = value.first[:Product], value.last[:_Sum_1]
    # Operations are an array of tuples: [['+', 2], ['-', 3]]
    operations.inject(product) {|accumulator, vv| accumulator.send(*vv)}
  end

  # (('+' | '-') Product)\*
  #
  #      (rule _Sum_2 "2.2" (seq _Sum_3 Product))
  #      (rule _Sum_3 "2.3" (alt "+" "-"))
  #
  # Turn [{_Sum_3: "+"}, {Product: N}] into ["+" N]
  production(:_Sum_2) do |value|
    value.map(&:values).flatten
  end

  # [3] Product := Power (('\*' | '/') Power)\*
  #
  #      (rule Product "3" (seq Power _Product_1))
  #      (rule _Product_1 "3.1" (star _Product_2))
  production(:Product, clear_packrat: true) do |value|
    power, operations = value.first[:Power], value.last[:_Product_1]
    # Operations are an array of tuples: [['*', 2], ['/', 3]]
    operations.inject(power) {|accumulator, vv| accumulator.send(*vv)}
  end

  # (('\*' | '/') Power)\*
  #
  #      (rule _Product_2 "3.2" (seq _Product_3 Power))
  #      (rule _Product_3 "3.3" (alt "*" "/"))
  #
  # Turn [{_Product_3: "*"}, {Power: N}] into ["*" N]
  production(:_Product_2) do |value|
    value.map(&:values).flatten
  end

  # [4] Power   := Value ('^' Power)?
  #
  #      (rule Power "4" (seq Value _Power_1))
  production(:Power, clear_packrat: true) do |value|
    val, pow = value.first[:Value], value.last[:_Power_1]
    pow ? val.pow(pow) : val
  end

  # ('^' Power)?
  #
  #      (rule _Power_2 "4.2" (seq "^" Power))
  production(:_Power_2) {|value| value.last[:Power]}

  # [5] Value   := [0-9]+ | '(' Expr ')'
  #
  #     (rule Value "5" (alt NUMBER _Value_1))
  #     (rule _Value_1 "5.1" (seq "(" Expr ")"))
  production(:Value, clear_packrat: true) do |value|
    case value
    when String then value.to_i
    when Array then value[1][:Expr]
    end
  end

  # Terminals don't require any special processing, but we could optimize by creating a regular expression such as `/\d+/`.
  #     (terminal NUMBER "6" (plus _NUMBER_1))
  #     (terminal _NUMBER_1 "6.1" (range "0-9")))

  # Instantiate the calculator using the EBNF grammar.
  #
  # @param  [Hash{Symbol => Object}] options
  # @option options [Boolean] :trace
  #   Trace level. 0(debug), 1(info), 2(warn), 3(error).
  def initialize(**options)
    # Intantiate grammar from ebnf.ebnf
    ebnf = File.expand_path("../calc.ebnf", __FILE__)

    # Perform PEG-specific transformation to the associated rules, which will be passed directly to the parser.
    @rules = EBNF.parse(File.open(ebnf)).make_peg.ast

    @options = options.dup

    # If the `trace` option is set, instantiate a logger for collecting trace information.
    if @options.has_key?(:trace)
      @options[:logger] = Logger.new(STDERR)
      @options[:logger].level = @options[:trace]
      @options[:logger].formatter = lambda {|severity, datetime, progname, msg| "#{severity} #{msg}\n"}
    end
  end

  # Evaluate an expression
  #
  # Evaluates each line of input.
  #
  # @param [String] input
  def evaluate(input)
    result = parse(input, :Expr, @rules, **@options)
      # This is called for each Expr
    puts result
  end
end
