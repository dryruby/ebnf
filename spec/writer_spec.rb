# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Writer do
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
          expect(EBNF::Writer.string(*subject)).to eq plain
        end
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
