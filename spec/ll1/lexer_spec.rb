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
    [:STRING_LITERAL_LONG_SINGLE_QUOTE, STRING_LITERAL_LONG_SINGLE_QUOTE],
    [:STRING_LITERAL_LONG_QUOTE,        STRING_LITERAL_LONG_QUOTE],
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
        output.force_encoding(Encoding::UTF_8) if output.respond_to?(:force_encoding) # Ruby 1.9+
        EBNF::LL1::Lexer.unescape_codepoints(input).should == output
      end
    end

    it "unescapes \\UXXXXXXXX codepoint escape sequences" do
      inputs = {
        %q(\\U00000020)   => %q( ),
        %q(\\U00010000)   => %Q(\xF0\x90\x80\x80),
        %q(\\U000EFFFF)   => %Q(\xF3\xAF\xBF\xBF),
      }
      inputs.each do |input, output|
        output.force_encoding(Encoding::UTF_8) if output.respond_to?(:force_encoding) # Ruby 1.9+
        EBNF::LL1::Lexer.unescape_codepoints(input).should == output
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
          EBNF::LL1::Lexer.unescape_codepoints(escaped).should == unescaped
        end
      end
    end
  end

  describe ".unescape_string" do
    # @see http://www.w3.org/TR/rdf-sparql-query/#grammarEscapes

    context "escape sequences" do
      EBNF::LL1::Lexer::ESCAPE_CHARS.each do |escaped, unescaped|
        it "unescapes #{unescaped.inspect}" do
          EBNF::LL1::Lexer.unescape_string(escaped).should == unescaped
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
          EBNF::LL1::Lexer.unescape_string(escaped).should == unescaped
        end
      end
    end
  end

  describe ".tokenize" do
    describe "numeric literals" do
      it "tokenizes unsigned integer literals" do
        tokenize(%q(42)) do |tokens|
          tokens.should have(1).element
          tokens.first.type.should  == :INTEGER
          tokens.first.value.should == "42"
        end
      end

      it "tokenizes positive integer literals" do
        tokenize(%q(+42)) do |tokens|
          tokens.should have(1).element
          tokens.last.type.should  == :INTEGER
          tokens.last.value.should == "+42"
        end
      end

      it "tokenizes negative integer literals" do
        tokenize(%q(-42)) do |tokens|
          tokens.should have(1).element
          tokens.last.type.should  == :INTEGER
          tokens.last.value.should == "-42"
        end
      end

      it "tokenizes unsigned decimal literals" do
        tokenize(%q(3.1415)) do |tokens|
          tokens.should have(1).element
          tokens.first.type.should  == :DECIMAL
          tokens.first.value.should == "3.1415"
        end
      end

      it "tokenizes positive decimal literals" do
        tokenize(%q(+3.1415)) do |tokens|
          tokens.should have(1).element
          tokens.last.type.should  == :DECIMAL
          tokens.last.value.should == "+3.1415"
        end
      end

      it "tokenizes negative decimal literals" do
        tokenize(%q(-3.1415)) do |tokens|
          tokens.should have(1).element
          tokens.last.type.should  == :DECIMAL
          tokens.last.value.should == "-3.1415"
        end
      end

      it "tokenizes unsigned double literals" do
        tokenize(%q(1e6)) do |tokens|
          tokens.should have(1).element
          tokens.first.type.should  == :DOUBLE
          tokens.first.value.should == "1e6"
        end
      end

      it "tokenizes positive double literals" do
        tokenize(%q(+1e6)) do |tokens|
          tokens.should have(1).element
          tokens.last.type.should  == :DOUBLE
          tokens.last.value.should == "+1e6"
        end
      end

      it "tokenizes negative double literals" do
        tokenize(%q(-1e6)) do |tokens|
          tokens.should have(1).element
          tokens.last.type.should  == :DOUBLE
          tokens.last.value.should == "-1e6"
        end
      end
    end

    describe "string terminals" do
      %w|^^ ( ) [ ] , ; . a true false @base @prefix|.each do |string|
        it "tokenizes the #{string.inspect} string" do
          tokenize(string) do |tokens|
            tokens.should have(1).element
            tokens.first.type.should  == nil
            tokens.first.value.should == string
          end
        end
      end
    end

    describe "comments" do
      it "ignores the remainder of the current line" do
        tokenize("# :foo :bar", "# :foo :bar\n", "# :foo :bar\r\n") do |tokens|
          tokens.should have(0).elements
        end
      end

      it "ignores leading whitespace" do
        tokenize(" # :foo :bar", "\n# :foo :bar", "\r\n# :foo :bar") do |tokens|
          tokens.should have(0).elements
        end
      end
    end

    describe "white space" do
      it "tracks the current line number" do
        inputs = {
          ""     => 1,
          "\n"   => 2,
          "\n\n" => 3,
          "\r\n" => 2,
        }
        inputs.each do |input, lineno|
          lexer = tokenize(input)
          lexer.to_a # consumes the input
          lexer.lineno.should == lineno
        end
      end
    end

    describe "yielding tokens" do
      it "annotates tokens with the current line number" do
        results = %w(1 2 3 4)
        tokenize("1\n2\n3\n4").each_token do |token|
          token.type.should == :INTEGER
          token.value.should == results.shift
        end
      end
    end

    describe "#first/#shift/#recover" do
      subject {tokenize("1\n2\n3\n4")}
      it "returns tokens in first/shift sequence" do
        %w{1 2 3 4}.each do |v|
          subject.first.value.should == v
          subject.shift
        end
        subject.first.should be_nil
      end

      context "with unrecognized token" do
        subject {tokenize("< space > 'foo' 1")}

        it "raises error with #first" do
          lambda {subject.first}.should raise_error(EBNF::LL1::Lexer::Error, /Invalid token/)
        end
        
        it "recovers to next token" do
          subject.recover
          subject.first.value.should == "'foo'"
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
      lexer = EBNF::LL1::Lexer.tokenize(input, terminals, :unescape_terms => unescape_terms)
      lexer.should be_a(EBNF::LL1::Lexer)
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
end
