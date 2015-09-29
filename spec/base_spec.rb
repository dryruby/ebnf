# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Base do
  describe ".new" do
    {
      %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*} =>
        %{((rule Prolog "2" (seq (opt BaseDecl) (star PrefixDecl))))},
      %{
        @terminals
        [3] terminal ::= [A-Z_]+
      } => %{((terminal terminal "3" (plus (range "A-Z_"))))},
      %{
        [9] primary     ::= HEX
                        |   RANGE
                        |   ENUM 
                        |   O_RANGE
                        |   O_ENUM
                        |   STRING1
                        |   STRING2
                        |   '(' expression ')'
        
      } => %{((rule primary "9" (alt HEX RANGE ENUM O_RANGE O_ENUM STRING1 STRING2 (seq "(" expression ")"))))},
      %{#[1] rule ::= 'FOO'} => %{()},
      %{//[1] rule ::= 'FOO'} => %{()},
      %{[18] SolutionModifier ::= _SolutionModifier_1 _SolutionModifier_2} =>
        %{((rule SolutionModifier "18" (seq _SolutionModifier_1 _SolutionModifier_2)))},
      %{[18.1]  _SolutionModifier_1 ::= _empty | GroupClause} =>
        %{((rule _SolutionModifier_1 "18.1" (alt _empty GroupClause)))},
      %q{[18] STRING1    ::= '"' (CHAR | [\t\'\[\]\(\)\-])* '"'} =>
        %q{((terminal STRING1 "18" (seq "\"" (star (alt CHAR (range "\t'[]()-"))) "\"")))},
      %q{[161s] WS ::= #x20 | #x9 | #xD | #xA} =>
        %q{((terminal WS "161s" (alt (hex "#x20") (hex "#x9") (hex "#xD") (hex "#xA"))))},
    }.each do |input, expected|
      it "parses #{input.inspect}" do
        expect(parse(input).to_sxp).to produce(expected, @debug)
      end

      it "parses generated SXP for #{input.inspect}" do
        ast = parse(expected, :format => :sxp).ast
        ast.each {|r| expect(r).to be_a(EBNF::Rule)}
        expect(ast.to_sxp).to produce(expected, @debug)
      end
    end
  end

  describe "#dup" do
    specify {expect(parse(%{[2]     Prolog    ::=           BaseDecl? PrefixDecl*}).dup).to be_a(EBNF::Base)}
  end

  def parse(value, options = {})
    @debug = []
    options = {:debug => @debug}.merge(options)
    EBNF::Base.new(value, options)
  end
end
