# coding: utf-8
$:.unshift ".."
require 'spec_helper'
require 'ebnf'

describe EBNF::LL1::Lexer do
  let!(:terminals) {[
    [:ANON,                             ANON],
    [nil,                               %r([\(\),.;\[\]a]|\^\^|@base|@prefix|true|false)],
    [:BLANK_NODE_LABEL,                 BLANK_NODE_LABEL],
    [:IRIREF,                           IRIREF],
    [:DECIMAL,                          DECIMAL],
    [:DOUBLE,                           DOUBLE],
    [:INTEGER,                          INTEGER],
    [:LANGTAG,                          LANGTAG],
    [:PNAME,                            PNAME],
    [:STRING_LITERAL_LONG_SINGLE_QUOTE, STRING_LITERAL_LONG_SINGLE_QUOTE, {partial_regexp: /'''/}],
    [:STRING_LITERAL_LONG_QUOTE,        STRING_LITERAL_LONG_QUOTE, {partial_regexp: /"""/}],
    [:STRING_LITERAL_QUOTE,             STRING_LITERAL_QUOTE],
    [:STRING_LITERAL_SINGLE_QUOTE,      STRING_LITERAL_SINGLE_QUOTE],
  ]}

  let!(:unescape_terms) {[
    :IRIREF,
    :STRING_LITERAL_QUOTE, :STRING_LITERAL_SINGLE_QUOTE,
    :STRING_LITERAL_LONG_SINGLE_QUOTE, :STRING_LITERAL_LONG_QUOTE
  ]}

  describe ".tokenize" do
    context "numeric literals" do
      it "tokenizes unsigned integer literals" do
        tokenize(%q(42)) do |tokens|
          expect(tokens.length).to eql 1
          expect(tokens.first.type).to eq :INTEGER
          expect(tokens.first.value).to eq "42"
        end
      end

      it "tokenizes positive integer literals" do
        tokenize(%q(+42)) do |tokens|
          expect(tokens.length).to eql 1
          expect(tokens.last.type).to eq :INTEGER
          expect(tokens.first.value).to eq "+42"
        end
      end

      it "tokenizes negative integer literals" do
        tokenize(%q(-42)) do |tokens|
          expect(tokens.length).to eql 1
          expect(tokens.last.type).to eq :INTEGER
          expect(tokens.first.value).to eq "-42"
        end
      end

      it "tokenizes unsigned decimal literals" do
        tokenize(%q(3.1415)) do |tokens|
          expect(tokens.length).to eql 1
          expect(tokens.first.type).to eq :DECIMAL
          expect(tokens.first.value).to eq "3.1415"
        end
      end

      it "tokenizes positive decimal literals" do
        tokenize(%q(+3.1415)) do |tokens|
          expect(tokens.length).to eql 1
          expect(tokens.last.type).to eq :DECIMAL
          expect(tokens.first.value).to eq "+3.1415"
        end
      end

      it "tokenizes negative decimal literals" do
        tokenize(%q(-3.1415)) do |tokens|
          expect(tokens.length).to eql 1
          expect(tokens.last.type).to eq :DECIMAL
          expect(tokens.first.value).to eq "-3.1415"
        end
      end

      it "tokenizes unsigned double literals" do
        tokenize(%q(1e6)) do |tokens|
          expect(tokens.length).to eql 1
          expect(tokens.first.type).to eq :DOUBLE
          expect(tokens.first.value).to eq "1e6"
        end
      end

      it "tokenizes positive double literals" do
        tokenize(%q(+1e6)) do |tokens|
          expect(tokens.length).to eql 1
          expect(tokens.last.type).to eq :DOUBLE
          expect(tokens.first.value).to eq "+1e6"
        end
      end

      it "tokenizes negative double literals" do
        tokenize(%q(-1e6)) do |tokens|
          expect(tokens.length).to eql 1
          expect(tokens.last.type).to eq :DOUBLE
          expect(tokens.first.value).to eq "-1e6"
        end
      end
    end

    context "string terminals" do
      %w|^^ ( ) [ ] , ; . a true false @base @prefix|.each do |string|
        it "tokenizes the #{string.inspect} string" do
          tokenize(string) do |tokens|
            expect(tokens.length).to eql 1
            expect(tokens.first.type).to eq nil
            expect(tokens.first.value).to eq string
          end
        end
      end
    end

    context "comments" do
      it "ignores the remainder of the current line" do
        tokenize("# :foo :bar", "# :foo :bar\n", "# :foo :bar\r\n") do |tokens|
          expect(tokens.length).to eql 0
        end
      end

      it "ignores leading whitespace" do
        tokenize(" # :foo :bar", "\n# :foo :bar", "\r\n# :foo :bar") do |tokens|
          expect(tokens.length).to eql 0
        end
      end
    end

    it "matches input longer than low water mark when buffer is low" do
      input = StringIO.new %("""123456789 123456789 """ """123456789 123456789 """)
      lexer = EBNF::LL1::Lexer.new(input, terminals,
                                   unescape_terms: unescape_terms,
                                   whitespace:     WHITESPACE,
                                   low_water:      20,
                                   high_water:     40)
      expect(lexer.shift.type).to eq :STRING_LITERAL_LONG_QUOTE
      expect(lexer.shift.type).to eq :STRING_LITERAL_LONG_QUOTE
    end
  end

  describe "#valid?" do
    it "validates legal input" do
      expect(tokenize(%q(:a "b" <c>))).to be_valid
    end

    it "invalidates illegal input" do
      expect(tokenize(%q(:a 'open))).not_to be_valid
    end
  end

  describe "#lineno" do
    it "for white space" do
      inputs = {
        ""     => 1,
        "\n"   => 2,
        "\n\n" => 3,
        "\r\n" => 2,
      }
      inputs.each do |input, lineno|
        lexer = tokenize(input)
        lexer.to_a # consumes the input
        expect(lexer.lineno).to eq lineno
      end
    end

    context "STRING_LITERAL_LONG_QUOTE" do
      it "tracks line numbers" do
        input = %(
        :Test a rdfs:Class ;
          rdfs:subClassOf mf:ManifestEntry;
          rdfs:label "Superclass of all CSVW tests" ;
          rdfs:comment """
            All CSVW tests have an input file referenced using `mf:action`. Positive
            and Negative Evaluation Tests also have a result file referenced using
            `mf:result` . Other tests may take different inputs and options as defined
            for each test class.
          """ ;
          :b :c .
        )
        expect(tokenize(input).to_a.map(&:lineno)).to include(
          2, 2, 2, 2,
          3, 3, 3,
          4, 4, 4,
          5, 5, 10,
          11, 11, 11
        )
      end
    end
  end

  describe "#first/#shift/#recover" do
    subject {tokenize("1\n2\n3\n4")}
    it "returns tokens in first/shift sequence" do
      %w{1 2 3 4}.each do |v|
        expect(subject.first.value).to eq v
        subject.shift
      end
      expect(subject.first).to be_nil
    end

    context "with unrecognized token" do
      subject {tokenize("< space > 'foo' 1")}

      it "raises error with #first" do
        expect {subject.first}.to raise_error(EBNF::LL1::Lexer::Error, /Invalid token/)
      end
      
      it "recovers to next token" do
        subject.recover
        expect(subject.first.value).to eq "'foo'"
      end
    end

    describe "#first" do
      it "returns a token when passed as an argument" do
        expect(subject.first(:INTEGER)).to be_a(EBNF::LL1::Lexer::Token)
      end

      it "does not return a token unless passed as an argument" do
        expect {subject.first(:Double)}.to raise_error(EBNF::LL1::Lexer::Error, 'Invalid token "1"')
      end
    end
  end

  describe EBNF::LL1::Lexer::Token do
    subject {described_class.new(:type, 'value', lineno: 1)}

    describe "#type" do
      its(:type) {is_expected.to eq :type}
    end

    describe "#value" do
      its(:value) {is_expected.to eq 'value'}
    end

    describe "#lineno" do
      its(:lineno) {is_expected.to eq 1}
    end

    describe "#[]" do
      it "returns type at 0 index" do
        expect(subject[0]).to eq :type
      end

      it "returns value at 1 index" do
        expect(subject[1]).to eq 'value'
      end

      it "returns nil for other indexes" do
        expect(subject[2]).to be_nil
      end
    end

    describe "#===" do
      specify {expect(subject).to be === :type}
      specify {expect(subject).to be === 'value'}
    end

    describe "#to_hash" do
      specify {expect(subject.to_hash).to eql({type: :type, value: 'value'})}
    end

    describe "#to_s" do
      specify {expect(subject.to_s).to eq ":type"}
    end

    describe "#representation" do
      specify {expect(subject.representation).to eq :type}
    end

    describe "#to_a" do
      specify {expect(subject.to_a).to eq [:type, 'value']}
    end
  end

  describe EBNF::LL1::Lexer::Terminal do
    {
      "returns itself with no map entry": {
        input: "FOO",
        map: {},
        expect: "FOO"
      },
      "returns map value if specified": {
        input: "FOO",
        map: {"foo" => 'bar'},
        expect: "bar"
      },
    }.each do |name, params|
      it name do
        term = described_class.new(:nil, params[:regexp], map: params[:map])
        expect(term.canonicalize(params[:input])).to eq params[:expect]
      end
    end
  end

  def tokenize(*inputs)
    options = inputs.last.is_a?(Hash) ? inputs.pop : {}
    lexer = nil
    inputs.each do |input|
      lexer = EBNF::LL1::Lexer.tokenize(input, terminals,
                                        unescape_terms: unescape_terms,
                                        whitespace:     WHITESPACE)
      expect(lexer).to be_a(EBNF::LL1::Lexer)
      yield lexer.to_a if block_given?
    end
    lexer
  end

  EXPONENT                         = /[eE][+-]?[0-9]+/

  ANON                             = /\[\s*\]/
  BLANK_NODE_LABEL                 = /_:(?:\w)*/
  IRIREF                           = /<\w*>/
  INTEGER                          = /[+-]?[0-9]+/
  DECIMAL                          = /[+-]?(?:[0-9]*\.[0-9]+)/
  DOUBLE                           = /[+-]?(?:[0-9]+\.[0-9]*#{EXPONENT}|\.?[0-9]+#{EXPONENT})/
  LANGTAG                          = /@[a-zA-Z]+(?:-[a-zA-Z0-9]+)*/
  PNAME                            = /\w*:\w*/
  STRING_LITERAL_QUOTE             = /'(?:[^\'\\\n\r])*'/
  STRING_LITERAL_SINGLE_QUOTE      = /"(?:[^\"\\\n\r])*"/
  STRING_LITERAL_LONG_SINGLE_QUOTE = /'''(?:(?:'|'')?(?:[^'\\]))*'''/m
  STRING_LITERAL_LONG_QUOTE        = /"""(?:(?:"|"")?(?:[^"\\]|#))*"""/m 
  WHITESPACE                       = /(\s|(?:#[^x]*$))+/m.freeze
end
