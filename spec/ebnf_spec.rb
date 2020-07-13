# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF do
  describe ".parse" do
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
      %{
        primary     ::= HEX
                    |   RANGE
                    |   O_RANGE
                    |   STRING1
                    |   STRING2
                    |   '(' expression ')'
      } => %{((rule primary (alt HEX RANGE O_RANGE STRING1 STRING2 (seq "(" expression ")"))))},
    }.each do |input, expected|
      context input do
        subject {EBNF.parse(input)}
        it "creates ast" do
          expect(subject.ast.to_sxp).to produce(expected, [])
        end

        it "#to_sxp" do
          expect(subject.to_sxp).to produce(expected)
        end

        it "#to_ttl" do
          expect(subject.to_ttl("ex", "http://example.org/")).not_to be_empty
        end

        it "#to_html" do
          expect(subject.to_html).not_to be_empty
        end

        it "#to_s" do
          expect(subject.to_s).not_to be_empty
        end
      end
    end

    context "README" do
      let(:ebnf) {PARSED_EBNF_GRAMMAR.dup}
      subject {ebnf}

      it "creates ast" do
        expect(subject.ast.to_sxp).not_to be_empty
      end

      it "#to_sxp" do
        expect(subject.to_sxp).not_to be_empty
      end

      it "#to_ttl" do
        expect(subject.to_ttl("ex", "http://example.org/")).not_to be_empty
      end

      it "#to_html" do
        expect(subject.to_html).not_to be_empty
      end

      it "#to_s" do
        expect(subject.to_s).not_to be_empty
      end

      context "LL1" do
        before {subject.make_bnf}

        before do
          subject.first_follow(:ebnf)
          subject.build_tables
        end

        it "#to_ruby" do
          expect {subject.to_ruby}.to write(:something).to(:output)
        end
      end

      context "PEG" do
        before {subject.make_peg}

        it "#to_ruby" do
          expect {subject.to_ruby}.to write(:something).to(:output)
        end
      end
    end
  end
end
