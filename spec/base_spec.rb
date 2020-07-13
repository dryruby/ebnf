# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'
require 'rdf/turtle'

describe EBNF::Base do
  subject {PARSED_EBNF_GRAMMAR.dup}

  describe "#initialize" do
    {
      %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*} =>
        %{((rule Prolog "2" (seq (opt BaseDecl) (star PrefixDecl))))},
      %{
        @terminals
        [3] terminal ::= [A-Z]+
      } => %{((terminals _terminals (seq))
              (terminal terminal "3" (plus (range "A-Z"))))},
      %{
        [9] primary     ::= HEX
                        |   RANGE
                        |   O_RANGE
                        |   STRING1
                        |   STRING2
                        |   '(' expression ')'
        
      } => %{((rule primary "9" (alt HEX RANGE O_RANGE STRING1 STRING2 (seq "(" expression ")"))))},
      %{#[1] rule ::= 'FOO'} => %{()},
      %{//[1] rule ::= 'FOO'} => %{()},
      %{[18] SolutionModifier ::= _SolutionModifier_1 _SolutionModifier_2} =>
        %{((rule SolutionModifier "18" (seq _SolutionModifier_1 _SolutionModifier_2)))},
      %{[18.1]  _SolutionModifier_1 ::= _empty | GroupClause} =>
        %{((rule _SolutionModifier_1 "18.1" (alt _empty GroupClause)))},
      %q{[18] STRING1    ::= '"' (CHAR - '"')* '"'} =>
        %q{((terminal STRING1 "18" (seq "\"" (star (diff CHAR "\"")) "\"")))},
      %q{[161s] WS ::= #x20 | #x9 | #xD | #xA} =>
        %q{((terminal WS "161s" (alt (hex "#x20") (hex "#x9") (hex "#xD") (hex "#xA"))))},
      %q{[1] shexDoc ::= directive* # leading CODE} =>
        %q{((rule shexDoc "1" (star directive)))},
      %q{[1] shexDoc ::= directive* /* leading CODE */} =>
        %q{((rule shexDoc "1" (star directive)))},
      %q{[1] shexDoc ::= directive* (* leading CODE *)} =>
        %q{((rule shexDoc "1" (star directive)))},
      %q{[1] shexDoc ::= directive* // leading CODE} =>
        %q{((rule shexDoc "1" (star directive)))},
      %q{[1] shexDoc ::= /* leading CODE */ directive*} =>
        %q{((rule shexDoc "1" (star directive)))},
      %q{[1] shexDoc ::= (* leading CODE *) directive*} =>
        %q{((rule shexDoc "1" (star directive)))},
      %q{[156s]  STRING_LITERAL1       ::= "'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'" /* #x27=' #x5C=\ #xA=new line #xD=carriage return */} =>
        %q{((terminal STRING_LITERAL1 "156s"
              (seq "'" (star (alt (range "^#x27#x5C#xA#xD") ECHAR UCHAR)) "'")) )}
    }.each do |input, expected|
      it "parses #{input.inspect}" do
        expect(parse(input).to_sxp).to produce(expected, @debug)
      end

      it "parses generated SXP for #{input.inspect}" do
        ast = parse(expected, format: :sxp).ast
        ast.each {|r| expect(r).to be_a(EBNF::Rule)}
        expect(ast.to_sxp).to produce(expected, @debug)
      end
    end

    it "rejects unknown format" do
      expect {parse("foo", format: :unknown)}.to raise_error "unknown input format :unknown"
    end
  end

  describe "#renumber!" do
    it "creates identifiers for grammars without identifiers" do
      gram = EBNF.parse("a ::= b d ::= e")
      gram.renumber!
      expect(gram.ast.map(&:id)).to eq %w{1 2}
    end

    it "renumbers grammars with identifiers" do
      gram = EBNF.parse("[10] a ::= b [20] d ::= e")
      gram.renumber!
      expect(gram.ast.map(&:id)).to eq %w{1 2}
    end
  end

  describe "#validate!" do
    let(:simple) {EBNF.parse("a ::= b")}
    it "notes invalid grammar" do
      expect do
        expect {simple.validate!}.to raise_error SyntaxError, "In rule a: No rule found for b"
      end.to write(:something).to(:error)
    end

    it "validates EBNF" do
      expect {subject.validate!}.not_to raise_error
    end
  end

  describe "#valid?" do
    let(:simple) {EBNF.parse("a ::= b")}
    it "notes invalid grammar" do
      expect do
        expect(simple.valid?).to be_falsey
      end.to write(:something).to(:error)
    end

    it "validates EBNF" do
      expect(subject).to be_valid
    end
  end

  describe "#each" do
    it "yields each rule" do
      rules = subject.ast.select {|r| r.rule?}
      expect {|b| subject.each(:rule, &b)}.to yield_control.exactly(rules.length).times
    end
    it "yields each terminal" do
      terminals = subject.ast.select {|r| r.terminal?}
      expect {|b| subject.each(:terminal, &b)}.to yield_control.exactly(terminals.length).times
    end
  end

  describe "#to_sxp" do
    specify {expect(subject.to_sxp).to include("(rule ebnf")}
  end

  describe "#to_s" do
    specify {expect(subject.to_s).to include("[1]  ebnf")}
  end

  describe "#to_html" do
    specify {expect(subject.to_s).to include("[1]  ebnf")}
  end

  describe "#to_ruby" do
    specify {expect {subject.to_ruby}.to write(:something).to(:output)}
  end

  describe "#to_ttl" do
    let(:reader) {RDF::Turtle::Reader.new(subject.to_ttl, base_uri: 'http://example.org/')}
    specify {expect(reader).to be_valid}
  end

  describe "#dup" do
    specify {expect(parse(%{[2]     Prolog    ::=           BaseDecl? PrefixDecl*}).dup).to be_a(EBNF::Base)}
  end

  describe "#find_rule" do
    it "finds ebnf" do
      expect(subject.find_rule(:ebnf).sym).to eql :ebnf
    end
  end

  def parse(value, **options)
    @debug = []
    options = {debug: @debug}.merge(options)
    EBNF::Base.new(value, **options)
  end
end
