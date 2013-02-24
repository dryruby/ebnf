# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Base do
  describe ".new" do
    {
      %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*} =>
        %{((Prolog "2" (kind rule) (seq (opt BaseDecl) (star PrefixDecl))))},
      %{
        @terminals
        [3] terminal ::= [A-Z_]+
      } => %{((terminal "3" (kind terminal) (plus (range "A-Z_"))))},
      %{
        [9] primary     ::= HEX
                        |   RANGE
                        |   ENUM 
                        |   O_RANGE
                        |   O_ENUM
                        |   STRING1
                        |   STRING2
                        |   '(' expression ')'
        
      } => %{((primary "9" (kind rule) (alt HEX RANGE ENUM O_RANGE O_ENUM STRING1 STRING2 (seq "(" expression ")"))))},
      %q{
        @pass           ::= (
                              [#x20#09#0d%0a]
                            | ('/*' ([^*] | '*' [^/])* '*/')
                            )+
        
      } => %q{((@pass "0" (kind pass) (plus (alt (range "#x20#09#0d%0a") (seq "/*" (star (alt (range "^*") (seq "*" (range "^/")))) "*/")))))},
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
