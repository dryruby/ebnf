# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::BNF do
  describe "#make_bnf" do
    {
      %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*} =>
      %{((rule _empty "0" (seq))
         (rule Prolog "2" (seq _Prolog_1 _Prolog_2))
         (rule _Prolog_1 "2.1" (cleanup opt) (alt _empty BaseDecl))
         (rule _Prolog_2 "2.2" (cleanup star) (alt _empty _Prolog_3))
         (rule _Prolog_3 "2.3" (cleanup merge) (seq PrefixDecl _Prolog_2)))},
      %{
        [9] primary     ::= HEX
                        |   RANGE
                        |   O_RANGE
                        |   STRING1
                        |   STRING2
                        |   '(' expression ')'
        
      } =>
      %{((rule _empty "0" (seq))
         (rule primary "9" (alt HEX RANGE O_RANGE STRING1 STRING2 _primary_1 ))
         (rule _primary_1 "9.1" (seq '(' expression ')')))},
      %{
        [1]  R1 ::= 1 2
        [2]  R2 ::= 1 2
      } =>
      %{((rule _empty "0" (seq))
         (terminal R1 "1" (seq 1 2))
         (terminal R2 "2" (seq 1 2)))}  
    }.each do |input, expected|
      it "parses #{input.inspect}" do
        expect(parse(input).make_bnf.ast.to_sxp).to produce(expected, @debug)
      end
    end

    context "EBNF Grammar" do
      subject {parse(File.read(File.expand_path("../../etc/ebnf.ebnf", __FILE__))).make_bnf}
      it "rule expressions should be flat, terminal or alt/seq" do
        subject.ast.each do |rule|
          case
          when !rule.rule? then true
          when !rule.expr.is_a?(Array) then true
          else
            expect("#{rule.sym}: #{rule.expr.first}").to match(/#{rule.sym}: (alt|seq)/)
          end
        end
      end
    end

    context "Turtle Grammar" do
      subject {parse(File.read(File.expand_path("../../etc/turtle.ebnf", __FILE__))).make_bnf}
      it "rule expressions should be flat, terminal or alt/seq" do
        subject.ast.each do |rule|
          case
          when rule.terminal? then true
          when !rule.expr.is_a?(Array) then true
          else
            expect("#{rule.sym}: #{rule.expr.first}").to match(/#{rule.sym}: (alt|seq)/)
          end
        end
      end
    end
  end

  def parse(value, **options)
    @debug = []
    options = {debug: @debug, format: :native}.merge(options)
    EBNF::Base.new(value, **options)
  end
end
