# coding: utf-8
$:.unshift ".."
require 'spec_helper'
require 'ebnf'

describe EBNF::Unescape do
  
  describe ".unescape_codepoints" do
    # @see https://www.w3.org/TR/rdf-sparql-query/#codepointEscape

    it "unescapes \\uXXXX codepoint escape sequences" do
      inputs = {
        %q(\\u0020)       => %q( ),
        %q(<ab\\u00E9xy>) => %Q(<ab\xC3\xA9xy>),
        %q(\\u03B1:a)     => %Q(\xCE\xB1:a),
        %q(a\\u003Ab)     => %Q(a\x3Ab),
      }
      inputs.each do |input, output|
        expect(EBNF::Unescape.unescape_codepoints(input)).to eq output
      end
    end

    it "unescapes \\UXXXXXXXX codepoint escape sequences" do
      inputs = {
        %q(\\U00000020)   => %q( ),
        %q(\\U00010000)   => %Q(\xF0\x90\x80\x80),
        %q(\\U000EFFFF)   => %Q(\xF3\xAF\xBF\xBF),
      }
      inputs.each do |input, output|
        expect(EBNF::Unescape.unescape_codepoints(input)).to eq output
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
          expect(EBNF::Unescape.unescape_codepoints(escaped)).to eq unescaped
        end
      end
    end
  end

  describe ".unescape_string" do
    # @see https://www.w3.org/TR/rdf-sparql-query/#grammarEscapes

    context "escape sequences" do
      EBNF::Unescape::ESCAPE_CHARS.each do |escaped, unescaped|
        it "unescapes #{unescaped.inspect}" do
          expect(EBNF::Unescape.unescape_string(escaped)).to eq unescaped
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
          expect(EBNF::Unescape.unescape_string(escaped)).to eq unescaped
        end
      end
    end
  end
end