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
    ParserTest.terminal(:unescape, /unescape/, unescape: true) {"foo"}
  }

  describe "ClassMethods" do
    describe "production" do
      it "adds as a start_handler" do
        expect(ParserTest.start_handlers.keys).to eq [:term]
        expect(ParserTest.start_handlers[:term]).to be_a(Proc)
      end
      it "adds as a production_handler" do
        expect(ParserTest.production_handlers.keys).to eq [:term]
        expect(ParserTest.production_handlers[:term]).to be_a(Proc)
      end
    end

    describe "terminal" do
      it "adds as a terminal_handler" do
        expect(ParserTest.terminal_handlers.keys).to include(:escape, :unescape)
        expect(ParserTest.terminal_handlers[:escape]).to be_a(Proc)
        expect(ParserTest.terminal_handlers[:unescape]).to be_a(Proc)
      end

      it "adds patterns" do
        expect(ParserTest.patterns).to include(
          EBNF::LL1::Lexer::Terminal.new(:escape, /escape/),
          EBNF::LL1::Lexer::Terminal.new(:unescape, /unescape/, unescape: true)
        )
      end
    end
  end

  describe "#parse" do
    subject {ParserTest.new}
    it "raises error if no branch table defined" do
      expect {subject.parse("foo")}.to raise_error(EBNF::LL1::Parser::Error, "Branch table not defined")
    end

    it "raises error if starting production not defined" do
      expect {
        subject.parse("foo", nil, branch: {a: {b: ["c"]}})
      }.to raise_error(EBNF::LL1::Parser::Error, "Starting production not defined")
    end
  end

  require_relative "data/parser"

  describe EBNFParser do
    let(:input) {File.expand_path("../../../etc/ebnf.ebnf", __FILE__)}
    let(:sxp) {File.read File.expand_path("../../../etc/ebnf.sxp", __FILE__)}
    let(:parser) {EBNFParser.new(File.open(input))}

    it "parses EBNF Grammar" do
      expect(parser.to_sxp).to produce(sxp, [])
    end
  end
end