# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Base do
  describe "#make_bnf" do
    {
      %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*} =>
      [%{(_empty "0" (kind rule) (seq))},
       %{(Prolog "2" (kind rule) (seq _Prolog_1 _Prolog_2))},
       %{(_Prolog_1 "2.1" (kind rule) (alt _empty BaseDecl))},
       %{(_Prolog_2 "2.2" (kind rule) (alt _empty __Prolog_2_star))},
       %{(__Prolog_2_star "2.2*" (kind rule) (seq PrefixDecl _Prolog_2))}],
      %{
        [9] primary     ::= HEX
                        |   RANGE
                        |   ENUM 
                        |   O_RANGE
                        |   O_ENUM
                        |   STRING1
                        |   STRING2
                        |   '(' expression ')'
        
      } =>
      [%{(_empty "0" (kind rule) (seq))},
       %{(primary "9" (kind rule) (alt HEX RANGE ENUM O_RANGE O_ENUM STRING1 STRING2 _primary_1))},
       %{(_primary_1 "9.1" (kind rule) (seq "(" expression ")"))}],
    }.each do |input, expected|
      it "parses #{input.inspect}" do
        parse(input).make_bnf.ast.map(&:to_s).should produce(expected, @debug)
      end
    end

    context "EBNF Grammar" do
      subject {parse(File.read(File.expand_path("../../etc/ebnf.bnf", __FILE__))).make_bnf}
      it "rule expressions should be flat, terminal or alt/seq" do
        subject.ast.each do |rule|
          case
          when rule.kind == :terminal then true
          when !rule.expr.is_a?(Array) then true
          else
            "#{rule.sym}: #{rule.expr.first}".should match(/#{rule.sym}: (alt|seq)/)
          end
        end
      end
    end

    context "Turtle Grammar" do
      subject {parse(File.read(File.expand_path("../../etc/turtle.bnf", __FILE__))).make_bnf}
      it "rule expressions should be flat, terminal or alt/seq" do
        subject.ast.each do |rule|
          case
          when rule.kind == :terminal then true
          when !rule.expr.is_a?(Array) then true
          else
            "#{rule.sym}: #{rule.expr.first}".should match(/#{rule.sym}: (alt|seq)/)
          end
        end
      end
    end
  end

  def parse(value, options = {})
    @debug = []
    options = {:debug => @debug}.merge(options)
    EBNF::Base.new(value, options)
  end
end
