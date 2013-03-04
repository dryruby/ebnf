# coding: utf-8
$:.unshift ".."
require 'spec_helper'
require 'ebnf'

describe EBNF::LL1::Parser do
  class ParserTest
    include EBNF::LL1::Parser
  end

  before(:all) {
    ParserTest.start_production(:term) {"foo"}
    ParserTest.production(:term) {"foo"}
    ParserTest.terminal(:escape, /escape/) {"foo"}
    ParserTest.terminal(:unescape, /unescape/, :unescape => true) {"foo"}
  }

  describe "ClassMethods" do
    describe "production" do
      it "adds as a start_handler" do
        ParserTest.start_handlers.keys.should == [:term]
        ParserTest.start_handlers[:term].should be_a(Proc)
      end
      it "adds as a production_handler" do
        ParserTest.production_handlers.keys.should == [:term]
        ParserTest.production_handlers[:term].should be_a(Proc)
      end
    end

    describe "terminal" do
      it "adds as a terminal_handler" do
        ParserTest.terminal_handlers.keys.should =~ [:escape, :unescape]
        ParserTest.terminal_handlers[:escape].should be_a(Proc)
        ParserTest.terminal_handlers[:unescape].should be_a(Proc)
      end

      it "adds patterns" do
        ParserTest.patterns.should =~ [
          EBNF::LL1::Lexer::Terminal.new(:escape, /escape/),
          EBNF::LL1::Lexer::Terminal.new(:unescape, /unescape/, :unescape => true)
        ]
      end
    end
  end

  describe "#parse" do
    subject {ParserTest.new}
    it "raises error if no branch table defined" do
      lambda {subject.parse("foo")}.should raise_error(EBNF::LL1::Parser::Error, "Branch table not defined")
    end

    it "raises error if starting production not defined" do
      lambda {
        subject.parse("foo", nil, :branch => {:a => {:b => ["c"]}})
      }.should raise_error(EBNF::LL1::Parser::Error, "Starting production not defined")
    end
  end
end