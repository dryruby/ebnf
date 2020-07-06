# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::ABNF do
  let(:logger) {RDF::Spec.logger}
  after(:each) do |example|
    puts logger.to_s if example.exception && !example.exception.is_a?(RSpec::Expectations::ExpectationNotMetError)
  end

  context "rule variations" do
    {
      "legal rule name": [
        'rulename = "foo"',
        %{((terminal rulename (istr "foo")))}
      ],
      "binary character": [
        "bin = %b11",
        %{((terminal bin (hex "#x3")))}
      ],
      "binary string": [
        "bin = %b1.10.11",
        %{((rule bin (seq (hex "#x1") (hex "#x2") (hex "#x3"))))}
      ],
      "binary string (ascii range)": [
        "bin = %b1100010.1101001.1101110",
        %{((rule bin (seq "bin")))}
      ],
      "binary string (mixed range)": [
        "bin = %b1100010.1.1101110",
        %{((rule bin (seq "b" (hex "#x1") "n")))}
      ],
      "decimal char": [
        "dec = %d22",
        %{((terminal dec (hex "#x16")))}
      ],
      "decimal string": [
        "dec = %d1.2.3",
        %{((rule dec (seq (hex "#x1") (hex "#x2") (hex "#x3"))))}
      ],
      "decimal string (ascii range)": [
        "dec = %d100.101.99",
        %{((rule dec (seq "dec")))}
      ],
      "decimal string (mixed range)": [
        "dec = %d100.1.99",
        %{((rule dec (seq "d" (hex "#x1") "c")))}
      ],
      "decimal range": [
        "dec = %d22-40",
        %{((terminal dec (range "#x16-#x28")))}
      ],
      "hex character": [
        "hex = %x1f",
        %{((terminal hex (hex "#x1f")))}
      ],
      "hex string": [
        "hex = %x1.a.c",
        %{((rule hex (seq (hex "#x1") (hex "#xa") (hex "#xc"))))}
      ],
      "hex string (ascii range)": [
        "hex = %x68.65.78",
        %{((rule hex (seq "hex")))}
      ],
      "hex string (mixed range)": [
        "hex = %x68.1.78",
        %{((rule hex (seq "h" (hex "#x1") "x")))}
      ],
      "hex range": [
        "hex = %x22-40",
        %{((terminal hex (range "#x22-#x40")))}
      ],
      "aliteration": [
        %(baz = foo / bar),
        %{((rule baz (alt foo bar)))}
      ],
      "incremental alternatives": [
        %(ruleset     =  alt1 / alt2\nruleset     =/ alt3\nruleset     =/ alt4 / alt5),
        %{((rule ruleset (alt alt1 alt2 alt3 alt4 alt5)))}
      ],
      "concatenated chars and ranges": [
        %(char-line = %x0D.0A %x20-7E %x0D.0A),
        %{((rule char-line (seq (seq (hex "#xd") (hex "#xa")) (range "#x20-#x7e") (seq (hex "#xd") (hex "#xa")))))}
      ],
      "sequence group": [
        %(sequence-group = elem (foo / bar) blat),
        %{((rule sequence-group (seq elem (alt foo bar) blat)))}
      ],
      "rept *": [
        %(rept = *A),
        %{((rule rept (star A)))}
      ],
      "rept 0*": [
        %(rept = 0*A),
        %{((rule rept (star A)))}
      ],
      "rept 1*": [
        %(rept = 1*A),
        %{((rule rept (plus A)))}
      ],
      "rept 2*": [
        %(rept = 2*A),
        %{((rule rept (rept 2 "*" A)))}
      ],
      "rept *1": [
        %(rept = *1A),
        %{((rule rept (rept 0 1 A)))}
      ],
      "rept 0*2": [
        %(rept = 0*2A),
        %{((rule rept (rept 0 2 A)))}
      ],
      "rept 1*3": [
        %(rept = 1*3A),
        %{((rule rept (rept 1 3 A)))}
      ],
      "rept 3": [
        %(rept = 3A),
        %{((rule rept (rept 3 3 A)))}
      ],
      "opt": [
        %(opt = [foo bar]),
        %{((rule opt (opt (seq foo bar))))}
      ],
      "comment": [
        %(foo         =  %x61           ; a),
        %{((terminal foo (hex "#x61")))}
      ],
      "prose-value": [
        %(prose = < free form >),
        %{((rule prose (seq "< free form >")))}
      ]
    }.each do |title, (input, expect)|
      it title do
        input << "\n" unless input.end_with?("\n")
        expect(parse(input).to_sxp).to produce(expect, logger)
      end
    end
  end

  context "Case-Sensitive String Support in ABNF" do
    {
      "case insensitive": [
        %(rulename = %i"aBc"),
        %{((terminal rulename (istr "aBc")))}
      ],
      "case sensitive": [
        %(rulename = %s"aBc"),
        %{((rule rulename (seq "aBc")))}
      ],
    }.each do |title, (input, expect)|
      it title do
        input << "\n" unless input.end_with?("\n")
        expect(parse(input).to_sxp).to produce(expect, logger)
      end
    end
  end

  context "Core Rules" do
    {
      "ALPHA": [
        "builtin = ALPHA",
        %{((rule builtin (seq ALPHA)) (terminal ALPHA (range "#x41-#x5A#x61-#x7A")))}
      ],
      "BIT": [
        "builtin = BIT",
        %{((rule builtin (seq BIT)) (terminal BIT (alt "0" "1")))}
      ],
      "CR": [
        "builtin = CR",
        %{((rule builtin (seq CR)) (terminal CR (hex "#x0D")))}
      ],
      "CRLF": [
        "builtin = CRLF",
        %{((rule builtin (seq CRLF)) (terminal CRLF (seq (opt CR) LF)))}
      ],
      "CTL": [
        "builtin = CTL",
        %{((rule builtin (seq CTL)) (terminal CTL (alt (range "#x00-#x1F") (hex "#x7F"))))}
      ],
      "DIGIT": [
        "builtin = DIGIT",
        %{((rule builtin (seq DIGIT)) (terminal DIGIT (range "#x30-#x39")))}
      ],
      "DQUOTE": [
        "builtin = DQUOTE",
        %{((rule builtin (seq DQUOTE)) (terminal DQUOTE (hex "#x22")))}
      ],
      "HEXDIG": [
        "builtin = HEXDIG",
        %{((rule builtin (seq HEXDIG)) (terminal HEXDIG (alt DIGIT (range "A-F"))))}
      ],
      "HTAB": [
        "builtin = HTAB",
        %{((rule builtin (seq HTAB)) (terminal HTAB (hex "#x09")))}
      ],
      "LF": [
        "builtin = LF",
        %{((rule builtin (seq LF)) (terminal LF (hex "#x0A")))}
      ],
      "LWSP": [
        "builtin = LWSP",
        %{((rule builtin (seq LWSP)) (terminal LWSP (star (alt WSP (seq CRLF WSP)))))}
      ],
      "WSP": [
        "builtin = WSP",
        %{((rule builtin (seq WSP)) (terminal WSP (alt SP HTAB)))}
      ],
    }.each do |title, (input, expect)|
      it title do
        input << "\n" unless input.end_with?("\n")
        expect(parse(input).to_sxp).to produce(expect, logger)
      end
    end
  end

  context "illegal syntax" do
    {
      "illegal rule name": "rule.name = CRLF\n",
      "no line ending": "rule.name = CRLF",
      "illegal binary": "bin = %b2\n",
      "illegal binary range": "bin = %b10-20\n",
      "illegal decimal": "dec = %d2f\n",
      "illegal decimal range": "dec = %d22-4060-80\n",
      "illegal hex": "hex = %x2g\n",
      "illegal hex range": "hex = %x22-4060-80\n",
    }.each do |title, input|
      it title do
        expect {parse(input)}.to raise_error(EBNF::PEG::Parser::Error)
      end
    end
  end

  it "parses ABNF grammar" do
    gram = parse(File.open(File.expand_path("../../etc/abnf.abnf", __FILE__)))
    expect(gram).to be_valid
  end

  def parse(input, **options)
    @debug = []
    EBNF.parse(input, debug: @debug, format: :abnf, **options)
  end
end
