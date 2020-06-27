# coding: utf-8
$:.unshift ".."
require 'spec_helper'
require 'ebnf'

describe EBNF::PEG::Rule do
  describe "#parse" do
    let(:parser) {double("PEG Parser", whitespace: /\s+/, packrat: {}, update_furthest_failure: true)}

    context "non-terminal rules" do
      {
        "(alt 'A' 'B') with 'A'" => {
          rule: [:alt, "A", "B"],
          input: "A",
          expect: "A"
        },
        "(alt 'A' 'B') with ' A '" => {
          rule: [:alt, "A", "B"],
          input: " A ",
          expect: "A"
        },
        "(alt 'A' 'B') with 'B'" => {
          rule: [:alt, "A", "B"],
          input: "B",
          expect: "B"
        },
        "(alt 'A' 'B') with 'C'" => {
          rule: [:alt, "A", "B"],
          input: "C",
          expect: :unmatched
        },
        "(not A) with 'A'" => {
          rule: [:not, "A"],
          input: "A",
          expect: :unmatched
        },
        "(not A) with 'B'" => {
          rule: [:not, "A"],
          input: "B",
          expect: nil
        },
        "(opt A) with 'A'" => {
          rule: [:opt, "A"],
          input: "A",
          expect: "A"
        },
        "(opt A) with 'A' and whitespace" => {
          rule: [:opt, "A"],
          input: " A",
          expect: "A"
        },
        "(opt A) with 'B'" => {
          rule: [:opt, "A"],
          input: "B",
          expect: nil
        },
        "(plus A) with ''" => {
          rule: [:plus, "A"],
          input: "",
          expect: :unmatched
        },
        "(plus A) with 'A'" => {
          rule: [:plus, "A"],
          input: "A",
          expect: %w(A)
        },
        "(plus A) with 'A B'" => {
          rule: [:plus, "A"],
          input: "A B",
          expect: %w(A)
        },
        "(plus A) with 'AAA'" => {
          rule: [:plus, "A"],
          input: "AAA",
          expect: %w(A A A)
        },
        "(plus A) with ' A A A '" => {
          rule: [:plus, "A"],
          input: " A A A ",
          expect: %w(A A A)
        },
        "(seq 'A' 'B')" => {
          rule: [:seq, "A", "B"],
          input: "A B",
          expect: [{A: "A"}, {B: "B"}]
        },
        "(seq 'A' 'B') with no whitespace" => {
          rule: [:seq, "A", "B"],
          input: "AB",
          expect: [{A: "A"}, {B: "B"}]
        },
        "(seq 'A' 'B') with added whitespace" => {
          rule: [:seq, "A", "B"],
          input: " A B ",
          expect: [{A: "A"}, {B: "B"}]
        },
        "(seq 'A' 'B') with 'A'" => {
          rule: [:seq, "A", "B"],
          input: " A ",
          expect: :unmatched
        },
        "(seq 'A' 'B') with 'AC'" => {
          rule: [:seq, "A", "B"],
          input: "AC",
          expect: :unmatched
        },
        "(star A) with ''" => {
          rule: [:star, "A"],
          input: "",
          expect: []
        },
        "(star A) with 'A'" => {
          rule: [:star, "A"],
          input: "A",
          expect: %w(A)
        },
        "(star A) with 'A B'" => {
          rule: [:star, "A"],
          input: "A B",
          expect: %w(A)
        },
        "(star A) with 'AAA'" => {
          rule: [:star, "A"],
          input: "AAA",
          expect: %w(A A A)
        },
        "(star A) with ' A A A '" => {
          rule: [:star, "A"],
          input: " A A A ",
          expect: %w(A A A)
        },
      }.each do |name, params|
        it name do
          rule = EBNF::Rule.new(:rule, "0", params[:rule]).extend(EBNF::PEG::Rule)
          rule.parser = parser
          expect(parser).to receive(:onStart).with(Symbol).and_return({})
          expect(parser).to receive(:onFinish).with(params[:expect]).and_return(params[:expect])
          expect(parser).not_to receive(:onTerminal).with(Symbol)

          expect(rule.parse(EBNF::LL1::Scanner.new(params[:input]))).to eql(params[:expect])
        end
      end

      context "with as_hash: true" do
        {
          "(alt 'A' 'B') with 'A'" => {
            rule: [:alt, "A", "B"],
            input: "A",
            expect: "A"
          },
          "(alt 'A' 'B') with ' A '" => {
            rule: [:alt, "A", "B"],
            input: " A ",
            expect: "A"
          },
          "(alt 'A' 'B') with 'B'" => {
            rule: [:alt, "A", "B"],
            input: "B",
            expect: "B"
          },
          "(alt 'A' 'B') with 'C'" => {
            rule: [:alt, "A", "B"],
            input: "C",
            expect: :unmatched
          },
          "(not A) with 'A'" => {
            rule: [:not, "A"],
            input: "A",
            expect: :unmatched
          },
          "(not A) with 'B'" => {
            rule: [:not, "A"],
            input: "B",
            expect: nil
          },
          "(opt A) with 'A'" => {
            rule: [:opt, "A"],
            input: "A",
            expect: "A"
          },
          "(opt A) with 'A' and whitespace" => {
            rule: [:opt, "A"],
            input: " A",
            expect: "A"
          },
          "(opt A) with 'B'" => {
            rule: [:opt, "A"],
            input: "B",
            expect: nil
          },
          "(plus A) with ''" => {
            rule: [:plus, "A"],
            input: "",
            expect: :unmatched
          },
          "(plus A) with 'A'" => {
            rule: [:plus, "A"],
            input: "A",
            expect: %w(A)
          },
          "(plus A) with 'A B'" => {
            rule: [:plus, "A"],
            input: "A B",
            expect: %w(A)
          },
          "(plus A) with 'AAA'" => {
            rule: [:plus, "A"],
            input: "AAA",
            expect: %w(A A A)
          },
          "(plus A) with ' A A A '" => {
            rule: [:plus, "A"],
            input: " A A A ",
            expect: %w(A A A)
          },
          "(seq 'A' 'B')" => {
            rule: [:seq, "A", "B"],
            input: "A B",
            expect: {A: "A", B: "B"}
          },
          "(seq 'A' 'B') with no whitespace" => {
            rule: [:seq, "A", "B"],
            input: "AB",
            expect: {A: "A", B: "B"}
          },
          "(seq 'A' 'B') with added whitespace" => {
            rule: [:seq, "A", "B"],
            input: " A B ",
            expect: {A: "A", B: "B"}
          },
          "(seq 'A' 'B') with 'A'" => {
            rule: [:seq, "A", "B"],
            input: " A ",
            expect: :unmatched
          },
          "(seq 'A' 'B') with 'AC'" => {
            rule: [:seq, "A", "B"],
            input: "AC",
            expect: :unmatched
          },
          "(star A) with ''" => {
            rule: [:star, "A"],
            input: "",
            expect: []
          },
          "(star A) with 'A'" => {
            rule: [:star, "A"],
            input: "A",
            expect: %w(A)
          },
          "(star A) with 'A B'" => {
            rule: [:star, "A"],
            input: "A B",
            expect: %w(A)
          },
          "(star A) with 'AAA'" => {
            rule: [:star, "A"],
            input: "AAA",
            expect: %w(A A A)
          },
          "(star A) with ' A A A '" => {
            rule: [:star, "A"],
            input: " A A A ",
            expect: %w(A A A)
          },
        }.each do |name, params|
          it name do
            rule = EBNF::Rule.new(:rule, "0", params[:rule]).extend(EBNF::PEG::Rule)
            rule.parser = parser
            expect(parser).to receive(:onStart).with(Symbol).and_return({as_hash: true})
            expect(parser).to receive(:onFinish).with(params[:expect]).and_return(params[:expect])
            expect(parser).not_to receive(:onTerminal).with(Symbol)

            expect(rule.parse(EBNF::LL1::Scanner.new(params[:input]))).to eql(params[:expect])
          end
        end
      end
    end

    context "terminal rules" do
      {
        "(diff 'A' 'A') with 'A'" => {
          rule: [:diff, "A", "A"],
          input: "A",
          expect: :unmatched
        },
        "(diff 'A' 'B') with 'A'" => {
          rule: [:diff, "A", "B"],
          input: "A",
          expect: "A"
        },
        "(diff 'A' 'B') with 'B'" => {
          rule: [:diff, "A", "B"],
          input: "B",
          expect: :unmatched
        },
        "(diff 'A' 'B') with ' A' (whitespace)" => {
          rule: [:diff, "A", "B"],
          input: " A",
          expect: :unmatched
        },
        "(hex #x41) with 'A'" => {
          rule: [:hex, "#x41"],
          input: "A",
          expect: "A"
        },
        "(hex #x41) with ' A' (whitespace)" => {
          rule: [:hex, "#x41"],
          input: " A",
          expect: :unmatched
        },
        "(hex #x41) with 'B'" => {
          rule: [:hex, "#x41"],
          input: "B",
          expect: :unmatched
        },
        "(range A-C) with 'A'" => {
          rule: [:range, "A-C"],
          input: "A",
          expect: "A"
        },
        "(range A-C) with ' A' (whitespace)" => {
          rule: [:range, "A-C"],
          input: " A",
          expect: :unmatched
        },
        "(range A-C) with 'B'" => {
          rule: [:range, "A-C"],
          input: "B",
          expect: "B"
        },
        "(range A-C) with 'D'" => {
          rule: [:range, "A-C"],
          input: "D",
          expect: :unmatched
        },
        "(range #x41-#x43) with 'A'" => {
          rule: [:range, "#x41-#x43"],
          input: "A",
          expect: "A"
        },
        "(range #x41-#x43) with ' A' (whitespace)" => {
          rule: [:range, "#x41-#x43"],
          input: " A",
          expect: :unmatched
        },
        "(range #x41-#x43) with 'B'" => {
          rule: [:range, "#x41-#x43"],
          input: "B",
          expect: "B"
        },
        "(range #x41-#x43) with 'D'" => {
          rule: [:range, "#x41-#x43"],
          input: "D",
          expect: :unmatched
        },
        "(range A-Ca-c) with 'a'" => {
          rule: [:range, "A-Ca-c"],
          input: "a",
          expect: "a"
        },
        "(range A-Ca-c) with ' a' (whitespace)" => {
          rule: [:range, "A-Ca-c"],
          input: " a",
          expect: :unmatched
        },
        "(range A-Ca-c) with 'b'" => {
          rule: [:range, "A-Ca-c"],
          input: "b",
          expect: "b"
        },
        "(range A-Ca-c) with 'd'" => {
          rule: [:range, "A-Ca-c"],
          input: "d",
          expect: :unmatched
        },
        "(range #x41-#x43#x61-#x63) with 'a'" => {
          rule: [:range, "#x41-#x43#x61-#x63"],
          input: "a",
          expect: "a"
        },
        "(range #x41-#x43#x61-#x63) with ' a' (whitespace)" => {
          rule: [:range, "#x41-#x43#x61-#x63"],
          input: " a",
          expect: :unmatched
        },
        "(range #x41-#x43#x61-#x63) with 'b'" => {
          rule: [:range, "#x41-#x43#x61-#x63"],
          input: "b",
          expect: "b"
        },
        "(range #x41-#x43#x61-#x63) with 'd'" => {
          rule: [:range, "#x41-#x43#x61-#x63"],
          input: "d",
          expect: :unmatched
        },
      }.each do |name, params|
        it name do
          rule = EBNF::Rule.new(:rule, "0", params[:rule], kind: :terminal).extend(EBNF::PEG::Rule)
          rule.parser = parser
          expect(parser).to receive(:onStart).with(Symbol).and_return({})
          expect(parser).to receive(:onFinish).with(params[:expect]).and_return(params[:expect])
          expect(parser).not_to receive(:onTerminal)
          expect(parser).to receive(:find_terminal_regexp).with(:rule)

          expect(rule.parse(EBNF::LL1::Scanner.new(params[:input]))).to eql(params[:expect])
        end
      end
    end
  end
end