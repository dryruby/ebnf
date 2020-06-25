# coding: utf-8
$:.unshift ".."
require 'spec_helper'
require 'ebnf'

describe EBNF::LL1::Parser do
  class LL1ParserTest
    include EBNF::LL1::Parser
  end

  before(:all) {
    LL1ParserTest.start_production(:term) {"foo"}
    LL1ParserTest.production(:term) {"foo"}
    LL1ParserTest.terminal(:escape, /escape/) {"foo"}
    LL1ParserTest.terminal(:unescape, /unescape/, unescape: true) {"foo"}
  }
  let(:logger) {RDF::Spec.logger}
  after(:each) do |example|
    puts logger.to_s if example.exception && !example.exception.is_a?(RSpec::Expectations::ExpectationNotMetError)
  end

  describe "ClassMethods" do
    describe "production" do
      it "adds as a start_handler" do
        expect(LL1ParserTest.start_handlers.keys).to eq [:term]
        expect(LL1ParserTest.start_handlers[:term]).to be_a(Proc)
      end
      it "adds as a production_handler" do
        expect(LL1ParserTest.production_handlers.keys).to eq [:term]
        expect(LL1ParserTest.production_handlers[:term]).to be_a(Proc)
      end
    end

    describe "terminal" do
      it "adds as a terminal_handler" do
        expect(LL1ParserTest.terminal_handlers.keys).to include(:escape, :unescape)
        expect(LL1ParserTest.terminal_handlers[:escape]).to be_a(Proc)
        expect(LL1ParserTest.terminal_handlers[:unescape]).to be_a(Proc)
      end

      it "adds patterns" do
        expect(LL1ParserTest.patterns).to include(
          EBNF::LL1::Lexer::Terminal.new(:escape, /escape/),
          EBNF::LL1::Lexer::Terminal.new(:unescape, /unescape/, unescape: true)
        )
      end
    end
  end

  describe "#parse" do
    subject {LL1ParserTest.new}
    it "raises error if no branch table defined" do
      expect {subject.parse("foo")}.to raise_error(EBNF::LL1::Parser::Error, "Branch table not defined")
    end

    it "raises error if starting production not defined" do
      expect {
        subject.parse("foo", nil, branch: {a: {b: ["c"]}})
      }.to raise_error(EBNF::LL1::Parser::Error, "Starting production not defined")
    end

    it "raises error on inalid input" do
      expect {
        subject.parse("bar", :foo, branch: {foo: {bar: ["baz"]}})
      }.to raise_error(EBNF::LL1::Parser::Error, /Invalid token "bar"/)
    end
  end

  require_relative "data/parser"

  describe EBNFParser do
    before {logger.level = Logger::INFO}
    let(:input) {File.expand_path("../../../etc/ebnf.ebnf", __FILE__)}
    let(:sxp) {File.read File.expand_path("../../../etc/ebnf.sxp", __FILE__)}
    let(:parser) {EBNFParser.new(File.open(input), debug: true, logger: logger)}

    it "parses EBNF Grammar" do
      expect(parser.to_sxp).to produce(sxp, logger)
    end
  end
end