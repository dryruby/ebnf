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
        %{[2] Prolog ::= BaseDecl? PrefixDecl*\n},
        %{<table class='grammar'>
            <tbody id='grammar-productions'>
              <tr id='grammar-production-Prolog'>
                <td>[2]</td>
                <td><code>Prolog</code></td>
                <td>::=</td>
                <td>
                  <a href="#grammar-production-BaseDecl">BaseDecl</a><code>?</code> 
                  <a href="#grammar-production-PrefixDecl">PrefixDecl</a><code>*</code>
                </td>
              </tr>
            </tbody>
          </table>
          }.gsub(/^          /, ''),
      ],
    }.each do |title, (grammar, plain, html)|
      context title do
        subject {EBNF::Base.new(grammar).ast}

        it "generates plain" do
          expect(EBNF::Writer.string(*subject)).to eq plain
        end

        it "generates HTML" do
          expect(EBNF::Writer.html(*subject)).to eq html
        end
      end
    end
  end
end
