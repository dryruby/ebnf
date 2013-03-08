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
    }.each do |input, expected|
      it "parses #{input.inspect}" do
        parse(input).ast.to_sxp.should produce(expected, @debug)
      end
    end
  end

  def parse(value, options = {})
    @debug = []
    options = {:debug => @debug}.merge(options)
    EBNF::Base.new(value, options)
  end
end
