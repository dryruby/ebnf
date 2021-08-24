# coding: utf-8
$:.unshift ".."
require 'spec_helper'
require 'ebnf'

describe EBNF::PEG::Parser do
  class PegParserTest
    include EBNF::PEG::Parser
  end

  before(:all) {
    PegParserTest.start_production(:term) {"foo"}
    PegParserTest.production(:term) {"foo"}
    PegParserTest.start_production(:toLower) {|value| value}
    PegParserTest.start_production(:toUpper) {|value| value}
    PegParserTest.terminal(:escape, /escape/) {"foo"}
    PegParserTest.terminal(:unescape, /unescape/, unescape: true) {"foo"}
  }
  let(:logger) {RDF::Spec.logger}
  after(:each) do |example|
    puts logger.to_s if example.exception && !example.exception.is_a?(RSpec::Expectations::ExpectationNotMetError)
  end

  describe "ClassMethods" do
    describe "production" do
      it "adds as a start_handler" do
        expect(PegParserTest.start_handlers.keys).to eq [:term]
        expect(PegParserTest.start_handlers[:term]).to be_a(Proc)
      end
      it "adds as a production_handler" do
        expect(PegParserTest.production_handlers.keys).to eq [:term]
        expect(PegParserTest.production_handlers[:term]).to include(Proc, FalseClass)
      end
    end

    describe "terminal" do
      it "adds as a terminal_handler" do
        expect(PegParserTest.terminal_handlers.keys).to include(:escape, :unescape)
        expect(PegParserTest.terminal_handlers[:escape]).to be_a(Proc)
        expect(PegParserTest.terminal_handlers[:unescape]).to be_a(Proc)
      end
    end
  end

  describe "#parse" do
    subject {PegParserTest.new}
    it "raises error if starting production not defined" do
      rule = EBNF::Rule.new(:rule, "0", [:seq, "foo"], kind: :terminal).extend(EBNF::PEG::Rule)
      expect {
        subject.parse("foo", "none", [rule])
      }.to raise_error(EBNF::PEG::Parser::Error, "Starting production :none not defined")
    end

    context "simplest grammar" do
      let(:start) {:expression}
      let(:grammar) {%{(
        (rule expression "1" (alt sum integer))
        (rule sum "2" (seq integer operator expression))
        (terminal operator "3" (seq "+"))
        (terminal integer "4" (plus (range "0-9")))
      )}}
      let(:rules) {EBNF.parse(grammar, format: :sxp).make_peg.ast}

      {
        "1" => "1",
        "10" => "10",
        "1+1" => [{integer: "1"}, {operator: "+"}, {expression: "1"}],
        " 1 +  2 " => [{integer: "1"}, {operator: "+"}, {expression: "2"}],
        "1 + 2 + 3" => [
          {integer: "1"},
          {operator: "+"},
          {expression: [
            {integer: "2"},
            {operator: "+"},
            {expression: "3"}
          ]}]
      }.each do |input, expected|
        it "parses #{input.inspect} to #{expected.inspect}" do
          output = PegParserTest.new.parse(input, start, rules, debug: 3, logger: logger)
          expect(output).to produce(expected, logger)
        end
      end

      {
        "" => %r{syntax error, expecting "0-9", :integer },
        "10 x 1" => %r{syntax error, expecting "\+", :operator},
        "1-1" => %r{syntax error, expecting "0-9", "\+", :operator},
        "foo" => %r{syntax error, expecting "0-9", :integer},
        "3 1 + 2" => %r{syntax error, expecting "\+", :operator}
      }.each do |input, expected|
        it "fails to parse #{input.inspect} to #{expected.inspect}" do
          expect {
            PegParserTest.new.parse(input, start, rules, debug: 3, logger: logger)
          }.to raise_error(EBNF::PEG::Parser::Error, expected)
        end
      end
    end

    context "case insensitive string matching" do
      let(:start) {:expression}
      let(:grammar) {%{(
        (rule expression "1" (alt upper lower))
        (rule upper "2" (seq "uPpEr"))
        (rule lower "3" (seq "LoWeR"))
      )}}
      let(:rules) {EBNF.parse(grammar, format: :sxp).make_peg.ast}

      {
        "UPPER" => "UPPER",
        "upper" => "UPPER",
        "LOWER" => "lower",
        "lower" => "lower",
      }.each do |input, expected|
        it "parses #{input.inspect} to #{expected.inspect}" do
          output = PegParserTest.new.parse(input, start, rules, debug: 3, logger: logger)
          expect(output).to produce(expected, logger)
        end
      end
    end

    context "with backtracking" do
      let(:start) {:expression}
      let(:grammar) {%{(
        (rule expression "1" (alt (seq integer "+" integer) (seq integer "*" integer)))
        (terminal integer "2" (plus (range "0-9")))
      )}}
      let(:rules) {EBNF.parse(grammar, format: :sxp).make_peg.ast}

      {
        "1+1" => [{integer: "1"}, {"+": "+"}, {integer: "1"}],
        # The following will memoize the first position
        "1*1" => [{integer: "1"}, {"*": "*"}, {integer: "1"}],
      }.each do |input, expected|
        it "parses #{input.inspect} to #{expected.inspect}" do
          output = PegParserTest.new.parse(input, start, rules, debug: 3, logger: logger)
          expect(output).to produce(expected, logger)
        end
      end
    end

    context "turtle grammar" do
      let(:start) {:turtleDoc}
      let(:grammar) {File.read File.expand_path("../../../etc/turtle.sxp", __FILE__)}
      let(:rules) {EBNF.parse(grammar, format: :sxp).make_peg.ast}

      {
        ":a :b :c ." => [[
          {:triples=>[
            {:subject=>":a"},
            {:predicateObjectList=>[
              {:verb=>[{:iri=>":b"}]},
              {:objectList=>[
                {:object=>":c"},
                {:_objectList_1=>[]}
              ]},
              {:_predicateObjectList_1=>[]}
            ]}
          ]},
          {:"."=>"."}
        ]],
        "[:b :c] ." => [[
          {:triples=>[
            {:blankNodePropertyList=>[
              {:"["=>"["},
              {:predicateObjectList=>[
                {:verb=>[{:iri=>":b"}]},
                {:objectList=>[
                  {:object=>":c"},
                  {:_objectList_1=>[]}
                ]},
                {:_predicateObjectList_1=>[]}]},
              {:"]"=>"]"}
            ]},
            {:_triples_3=>nil}
          ]},
          {:"."=>"."}
        ]]
      }.each do |input, expected|
        it "parses #{input.inspect} to #{expected.inspect}" do
          output = PegParserTest.new.parse(input, start, rules, debug: 3, logger: logger)
          expect(output).to produce(expected, logger)
        end
      end
    end
  end

  require_relative "data/parser"

  describe EBNFPegParser do
    let(:input) {File.expand_path("../../../etc/ebnf.ebnf", __FILE__)}
    let(:sxp) {File.read File.expand_path("../../../etc/ebnf.sxp", __FILE__)}
    let(:parser) {EBNFPegParser.new(File.open(input), debug: 3, logger: logger)}

    it "parses EBNF Grammar" do
      expect(parser.to_sxp).to produce(sxp, logger)
    end
  end
end