# coding: utf-8
require 'spec_helper'
$:.unshift(File.expand_path("../../../examples/ebnf-parser", __FILE__))
require 'parser'

describe EBNFParser do
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
      %q{[18] STRING1    ::= '"' ((CHAR - '"') | '\\t')* '"'} =>
        %q{((terminal STRING1 "18" (seq "\"" (star (alt (diff CHAR "\"") "\t")) "\"")))}
    }.each do |input, expected|
      it "parses #{input.inspect}" do
        expect(parse(input, :validate => true).ast.to_sxp).to produce(expected, @debug)
      end
    end
  end

  def parse(value, options = {})
    @debug = []
    options = {:debug => @debug}.merge(options)
    EBNFParser.new(value, options)
  end
end