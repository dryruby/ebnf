# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'
require 'nokogiri'

describe EBNF::Writer do
  RSpec::Matchers.define :have_xpath do |path, value|
    match do |actual|
      doc = Nokogiri::HTML.parse(actual)
      return false unless doc
      @result = doc.at_xpath(path.to_s) rescue false
      case value
      when false
        @result.nil?
      when true
        !@result.nil?
      when Array
        @result.to_s.split(" ").include?(*value)
      when Regexp
        @result.to_s =~ value
      else
        @result.to_s == value
      end
    end

    failure_message do |actual|
      msg = "expected that #{path.inspect}\nwould be: #{value.inspect}"
      msg += "\n     was: #{@result}"
      msg += "\nsource:" + actual
      msg
    end

    failure_message_when_negated do |actual|
      msg = "expected that #{path.inspect}\nwould not be #{value.inspect}"
      msg += "\nsource:" + actual
      msg
    end
  end

  describe "#initialize" do
    {
      prolog: [
        %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*},
        %{[2] Prolog ::= BaseDecl? PrefixDecl*\n}
      ],
    }.each do |title, (grammar, plain)|
      context title do
        subject {EBNF::Base.new(grammar).ast}

        it "generates plain" do
          expect {EBNF::Writer.new(subject)}.to write(plain).to(:output)
        end
      end
    end
  end

  describe ".string" do
    {
      prolog: [
        %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*},
        %{[2] Prolog ::= BaseDecl? PrefixDecl*\n}
      ],
    }.each do |title, (grammar, plain)|
      context title do
        subject {EBNF::Base.new(grammar).ast}

        it "generates plain" do
          expect(EBNF::Writer.string(*subject)).to eq plain
        end
      end
    end
  end

  describe ".print" do
    {
      prolog: [
        %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*},
        %{[2] Prolog ::= BaseDecl? PrefixDecl*\n}
      ],
    }.each do |title, (grammar, plain)|
      context title do
        subject {EBNF::Base.new(grammar).ast}

        it "generates plain" do
          expect {EBNF::Writer.print(*subject)}.to write(plain).to(:output)
        end
      end
    end
  end

  describe ".html" do
    {
      prolog: [
        %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*},
        {
          '//table/@class': "grammar",
          '//table/tbody/@id': "grammar-productions",
          '//tbody/tr/@id': "grammar-production-Prolog",
          '//tbody/tr/td[1]/text()': "[2]",
          '//tbody/tr/td[2]/code/text()': "Prolog",
          '//tbody/tr/td[3]/text()': "::=",
          '//tbody/tr/td[4]/text()': /BaseDecl\? PrefixDecl\*/,
        }
      ],
    }.each do |title, (grammar, xpaths)|
      context title do
        subject {EBNF::Writer.html(*EBNF::Base.new(grammar).ast)}

        xpaths.each do |path, value|
          specify {is_expected.to have_xpath(path, value)}
        end
      end
    end
  end

  describe "#format_ebnf" do
    subject {EBNF::Writer.new([])}

    {
      "alt": [
        [:alt, :A, :B],
        "A | B"
      ],
      "enum": [
        [:range, "abc-"],
        "[abc-]"
      ],
      "hex": [
        [:hex, "#x20"],
        "#x20"
      ],
      "istr": [
        [:istr, "foo"],
        %("foo")
      ],
      "opt": [
        [:opt, :A],
        "A?"
      ],
      "plus": [
        [:plus, :A],
        "A+"
      ],
      "range": [
        [:range, "a-z"],
        "[a-z]"
      ],
      "rept 0 1": [
        [:rept, 0, 1, :A],
        "A?"
      ],
      "rept 0 *": [
        [:rept, 0, '*', :A],
        "A*"
      ],
      "rept 1 1": [
        [:rept, 1, 1, :A],
        "A"
      ],
      "rept 1 *": [
        [:rept, 1, '*', :A],
        "A+"
      ],
      "rept 1 2": [
        [:rept, 1, 2, :A],
        "A A?"
      ],
      "rept 1 3": [
        [:rept, 1, 3, :A],
        "A (A A?)?"
      ],
      "rept 1 3 (A B)": [
        [:rept, 1, 3, [:seq, :A, :B]],
        "(A B) ((A B) (A B)?)?"
      ],
      "rept 1 3 (A | B)": [
        [:rept, 1, 3, [:alt, :A, :B]],
        "(A | B) ((A | B) (A | B)?)?"
      ],
      "star": [
        [:star, :A],
        "A*"
      ],
    }.each do |title, (expr, result)|
      it title do
        expect(subject.send(:format_ebnf, expr)).to eql result
      end
    end
  end

  context "Existing grammars" do
    {
      "EBNF Grammar" => File.expand_path("../../etc/ebnf.ebnf", __FILE__),
      "Turtle Grammar" => File.expand_path("../../etc/turtle.ebnf", __FILE__)
    }.each do |name, file|
      context name do
        it "outputs grammar as text" do
          expect {EBNF.parse(File.read(file)).to_s}.to_not raise_error
        end
        it "outputs grammar as html" do
          expect {EBNF.parse(File.read(file)).to_html}.to_not raise_error
        end
      end
    end
  end
end
