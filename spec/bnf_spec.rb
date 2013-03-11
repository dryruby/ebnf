# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Base do
  describe "#make_bnf" do
    {
      %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*} =>
      %{((rule _empty "0" (seq))
         (rule Prolog "2" (seq _Prolog_1 _Prolog_2))
         (rule _Prolog_1 "2.1" (alt _empty BaseDecl))
         (rule _Prolog_2 "2.2" (alt _empty _Prolog_3))
         (rule _Prolog_3 "2.3" (seq PrefixDecl _Prolog_2)))},
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
      %{((rule _empty "0" (seq))
         (rule primary "9" (alt HEX RANGE ENUM O_RANGE O_ENUM STRING1 STRING2 _primary_1 ))
         (rule _primary_1 "9.1" (seq "(" expression ")")))},
    }.each do |input, expected|
      it "parses #{input.inspect}" do
        parse(input).make_bnf.ast.to_sxp.should produce(expected, @debug)
      end
    end

    context "EBNF Grammar" do
      subject {parse(File.read(File.expand_path("../../etc/ebnf.ebnf", __FILE__))).make_bnf}
      it "rule expressions should be flat, terminal or alt/seq" do
        subject.ast.each do |rule|
          case
          when rule.terminal? then true
          when !rule.expr.is_a?(Array) then true
          else
            "#{rule.sym}: #{rule.expr.first}".should match(/#{rule.sym}: (alt|seq)/)
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
