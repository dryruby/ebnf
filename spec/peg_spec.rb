# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::PEG do
  describe "#make_peg" do
    {
      %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*} =>
      %{((rule Prolog "2" (seq _Prolog_1 _Prolog_2))
         (rule _Prolog_1 "2.1" (opt BaseDecl))
         (rule _Prolog_2 "2.2" (star PrefixDecl)))},
      %{
        [9] primary     ::= HEX
                        |   RANGE
                        |   O_RANGE
                        |   STRING1
                        |   STRING2
                        |   '(' expression ')'
        
      } =>
        %{((rule primary "9" (alt HEX RANGE O_RANGE STRING1 STRING2 _primary_1))
           (rule _primary_1 "9.1" (seq "(" expression ")")))},
      %{[1] start ::= A B C} =>
        %{((rule start "1" (seq A B C)))},
      %{[1] start ::= A B? C* D+} =>
        %{((rule start "1" (seq A _start_1 _start_2 _start_3))
           (rule _start_1 "1.1" (opt B))
           (rule _start_2 "1.2" (star C))
           (rule _start_3 "1.3" (plus D)))},
      %{[1] start ::= A (B C) D} =>
        %{((rule start "1" (seq A _start_1 D))
           (rule _start_1 "1.1" (seq B C)))},
      %{[1] start ::= A (B) C} =>
        %{((rule start "1" (seq A B C)))},
      %{[1] start ::= A (B (C D)) (E F)} =>
        %{((rule start "1" (seq A _start_1 _start_2))
           (rule _start_1 "1.1" (seq B _start_3))
           (rule _start_3 "1.3" (seq C D))
           (rule _start_2 "1.2" (seq E F)))},
      %{[1] r1 ::= (A B) C
        [2] r2 ::= (A B) E} =>
        %{((rule r1 "1" (seq _r1_1 C))
           (rule _r1_1 "1.1" (seq A B))
           (rule r2 "2" (seq _r2_1 E))
           (rule _r2_1 "2.1" (seq A B)))}
    }.each do |input, expected|
      it "parses #{input.inspect}" do
        expect(parse(input).make_peg.ast.to_sxp).to produce(expected, @debug)
      end
    end
  end

  def parse(value, **options)
    @debug = []
    options = {debug: @debug}.merge(options)
    EBNF::Base.new(value, **options)
  end
end
