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
  
  describe ".unescape_codepoints" do
    # @see http://www.w3.org/TR/rdf-sparql-query/#codepointEscape

    it "unescapes \\uXXXX codepoint escape sequences" do
      inputs = {
        %q(\\u0020)       => %q( ),
        %q(<ab\\u00E9xy>) => %Q(<ab\xC3\xA9xy>),
        %q(\\u03B1:a)     => %Q(\xCE\xB1:a),
        %q(a\\u003Ab)     => %Q(a\x3Ab),
      }
      inputs.each do |input, output|
        output.force_encoding(Encoding::UTF_8)
        expect(EBNF::LL1::Lexer.unescape_codepoints(input)).to eq output
      end
    end

    it "unescapes \\UXXXXXXXX codepoint escape sequences" do
      inputs = {
        %q(\\U00000020)   => %q( ),
        %q(\\U00010000)   => %Q(\xF0\x90\x80\x80),
        %q(\\U000EFFFF)   => %Q(\xF3\xAF\xBF\xBF),
      }
      inputs.each do |input, output|
        output.force_encoding(Encoding::UTF_8)
        expect(EBNF::LL1::Lexer.unescape_codepoints(input)).to eq output
      end
    end

    
    context "escaped strings" do
      {
        'Dürst' => 'D\\u00FCrst',
        "é" => '\\u00E9',
        "€" => '\\u20AC',
        "resumé" => 'resum\\u00E9',
      }.each_pair do |unescaped, escaped|
        it "unescapes #{unescaped.inspect}" do
          expect(EBNF::LL1::Lexer.unescape_codepoints(escaped)).to eq unescaped
        end
      end
    end
  end

  describe ".unescape_string" do
    # @see http://www.w3.org/TR/rdf-sparql-query/#grammarEscapes

    context "escape sequences" do
      EBNF::LL1::Lexer::ESCAPE_CHARS.each do |escaped, unescaped|
        it "unescapes #{unescaped.inspect}" do
          expect(EBNF::LL1::Lexer.unescape_string(escaped)).to eq unescaped
        end
      end
    end
    
    context "escaped strings" do
      {
        'simple literal' => 'simple literal',
        'backslash:\\' => 'backslash:\\\\',
        'dquote:"' => 'dquote:\\"',
        "newline:\n" => 'newline:\\n',
        "return\r" => 'return\\r',
        "tab:\t" => 'tab:\\t',
      }.each_pair do |unescaped, escaped|
        it "unescapes #{unescaped.inspect}" do
          expect(EBNF::LL1::Lexer.unescape_string(escaped)).to eq unescaped
        end
      end
    end
  end

  describe ".tokenize" do
    describe "numeric literals" do
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

    describe "string terminals" do
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

    describe "comments" do
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

    describe "line numbers" do
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

    describe "yielding tokens" do
      it "annotates tokens with the current line number" do
        results = %w(1 2 3 4)
        tokenize("1\n2\n3\n4").each_token do |token|
          expect(token.type).to eq :INTEGER
          expect(token.value).to eq results.shift
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
    end

    describe EBNF::LL1::Lexer::Terminal do
      it "needs specs to check for canonicalization"
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
  WHITESPACE                       = /(\s|(?:#.*$))+/m.freeze
end
