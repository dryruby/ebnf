# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF do
  describe ".parse" do
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
      %q{
        @pass           ::= (
                              [#x20#09#0d%0a]
                            | ('/*' ([^*] | '*' [^/])* '*/')
                            )+
        
      } => %q{((pass @pass "0" (plus (alt (range "#x20#09#0d%0a") (seq "/*" (star (alt (range "^*") (seq "*" (range "^/")))) "*/" )))))},
    }.each do |input, expected|
      it "parses #{input.inspect}" do
        @debug = []
        EBNF.parse(input).ast.to_sxp.should produce(expected, @debug)
      end
    end
  end
end
