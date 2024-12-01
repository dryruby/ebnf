# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Parser do
  let(:logger) {RDF::Spec.logger}
  after(:each) do |example|
    puts logger.to_s if example.exception && !example.exception.is_a?(RSpec::Expectations::ExpectationNotMetError)
  end

  context "rule variations" do
    {
      "legal rule name": [
        'rulename ::= "foo"',
        %{((rule rulename (seq "foo")))}
      ],
      "prolog": [
        %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*},
        %{((rule Prolog "2" (seq (opt BaseDecl) (star PrefixDecl))))}
      ],
      "aliteration": [
        %{[2] declaration ::= '@terminals' | '@pass'},
        %{((rule declaration "2" (alt '@terminals' '@pass')))},
      ],
      "posfix": [
        %{[9] postfix     ::= primary ( [?*+] )?},
        %{((rule postfix "9" (seq primary (opt (range "?*+")))))},
      ],
      "diff": [
        %{[18] STRING2    ::= "'" (CHAR - "'")* "'"},
        %{((terminal STRING2 "18" (seq "'" (star (diff CHAR "'")) "'")))},
      ],
      "IRIREF": [
        %([18] IRIREF     ::= '<' ([^<>"{}|^`\]-[#x00-#x20] | UCHAR)* '>'),
        %{((terminal IRIREF "18"
            (seq '<'
              (star
                (alt
                  (diff (range "^<>\\\"{}|^`") (range "#x00-#x20"))
                UCHAR))
              '>')))},
      ],
      "minimal whitespace": [
        %{[xx]minimal::=whitespace[yy]whitespace::=" "},
        %{((rule minimal "xx" (seq whitespace))
           (rule whitespace "yy" (seq " ")))}
      ]
    }.each do |title, (input, expect)|
      it title do
        expect(parse(input).to_sxp).to produce(expect, logger)
      end
    end

    context "without rule identifiers" do
      {
        "prolog": [
          %{Prolog    ::=           BaseDecl? PrefixDecl*},
          %{((rule Prolog (seq (opt BaseDecl) (star PrefixDecl))))}
        ],
        "aliteration": [
          %{declaration ::= '@terminals' | '@pass'},
          %{((rule declaration (alt '@terminals' '@pass')))},
        ],
        "posfix": [
          %{postfix     ::= primary ( [?*+] )?},
          %{((rule postfix (seq primary (opt (range "?*+")))))},
        ],
        "diff": [
          %{STRING2    ::= "'" (CHAR - "'")* "'"},
          %{((terminal STRING2 (seq "'" (star (diff CHAR "'")) "'")))},
        ],
        "IRIREF": [
          %(IRIREF     ::= '<' ([^<>"{}|^`\]-[#x00-#x20] | UCHAR)* '>'),
          %{((terminal IRIREF
              (seq '<'
                (star
                  (alt
                    (diff (range "^<>\\\"{}|^`") (range "#x00-#x20"))
                  UCHAR))
                '>')))},
        ],
      }.each do |title, (input, expect)|
        it title do
          expect(parse(input).to_sxp).to produce(expect, logger)
        end
      end
    end
  end

  describe "#expression" do
    {
      "'abc' def" => %{(seq 'abc' def)},
      %{[0-9]} => %{(range "0-9")},
      %{#x00B7} => %{(hex "#x00B7")},
      %{[#x0300-#x036F]} => %{(range "#x0300-#x036F")},
      %{[^<>'{}|^`]-[#x00-#x20]} => %{(diff (range "^<>'{}|^`") (range "#x00-#x20"))},
      %{a b c} => %{(seq a b c)},
      %{a? b c} => %{(seq (opt a) b c)},
      %{a - b} => %{(diff a b)},
      %{(a - b) - c} => %{(diff (diff a b) c)},
      %{a b? c} => %{(seq a (opt b) c)},
      %{a | b | c} => %{(alt a b c)},
      %{a? b+ c*} => %{(seq (opt a) (plus b) (star c))},
      %{foo | x xlist} => %{(alt foo (seq x xlist))},
      %{a | (b - c)} => %{(alt a (diff b c))},
      %{a b | c d} => %{(alt (seq a b) (seq c d))},
      %{[a-z]} => %{(range "a-z")},
      %{[a-zA-Z]} => %{(range "a-zA-Z")},
      %{[#x20-#x22]} => %{(range "#x20-#x22")},
      %{[abc]} => %{(range "abc")},
      %{[abc-]} => %{(range "abc-")},
      %{[#x20#x21#x22]} => %{(range "#x20#x21#x22")},
      %{BaseDecl? PrefixDecl*} => %{(seq (opt BaseDecl) (star PrefixDecl))},
      %{NCCHAR1 | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]} =>
        %{(alt NCCHAR1 '-' (range "0-9") (hex "#x00B7") (range "#x0300-#x036F") (range "#x203F-#x2040"))},
      %{'<' ([^<>"{}|^`\]-[#x00-#x20] | UCHAR)* '>'} =>
        %{(seq '<' (star (alt (diff (range "^<>\\\"{}|^`") (range "#x00-#x20")) UCHAR)) '>')},
    }.each do |input, expected|
      it "given #{input.inspect} produces #{expected}" do
        rule = parse("rule ::= #{input}").ast.first
        expect(rule.expr.to_sxp).to produce(expected, @debug)
      end
    end
  end

  context "illegal syntax" do
    {
      "illegal rule name": %{$rule.name ::= foo},
      "diff missing second operand": %{rule ::= a -},
      "unrecognized terminal" => %{rule ::= %foo%},
      "unopened paren" => %{rule ::= a) b c}
    }.each do |title, input|
      it title do
        expect {parse(input)}.to raise_error(SyntaxError)
      end
    end
  end

  it "parses EBNF grammar" do
    gram = parse(File.open(File.expand_path("../../etc/ebnf.ebnf", __FILE__)))
    expect(gram).to be_valid
  end

  def parse(input, **options)
    @debug = []
    EBNF.parse(input, debug: @debug, format: :ebnf, **options)
  end
end
