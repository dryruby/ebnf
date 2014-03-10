# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Base do
  describe "#ruleParts" do
    {
      %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*} =>
        %{(rule Prolog "2" (seq (opt BaseDecl) (star PrefixDecl)))},
      %{[2] declaration ::= '@terminals' | '@pass'} =>
        %{(rule declaration "2" (alt "@terminals" "@pass"))},
      %{[9] postfix     ::= primary ( [?*+] )?} =>
        %{(rule postfix "9" (seq primary (opt (range "?*+"))))},
      %{[18] STRING2    ::= "'" (CHAR - "'")* "'"} =>
        %{(terminal STRING2 "18" (seq "'" (star (diff CHAR "'")) "'"))},
    }.each do |input, expected|
      it "given #{input.inspect} produces #{expected}" do
        expect(ebnf(:ruleParts, input).to_sxp).to produce(expected, @debug)
      end
    end
  end
  
  describe "#expression" do
    {
      "'abc' def" => %{((seq "abc" def) "")},
      %{[0-9]} => %{((range "0-9") "")},
      %{#x00B7} => %{((hex "#x00B7") "")},
      %{[#x0300-#x036F]} => %{((range "#x0300-#x036F") "")},
      %{[^<>'{}|^`]-[#x00-#x20]} => %{((diff (range "^<>'{}|^`") (range "#x00-#x20")) "")},
      %{a b c} => %{((seq a b c) "")},
      %{a? b c} => %{((seq (opt a) b c) "")},
      %(a - b) => %{((diff a b) "")},
      %((a - b) - c) => %{((diff (diff a b) c) "")},
      %(a b c) => %{((seq a b c) "")},
      %(a b? c) => %{((seq a (opt b) c) "")},
      %(a | b | c) => %{((alt a b c) "")},
      %(a? b+ c*) => %{((seq (opt a) (plus b) (star c)) "")},
      %( | x xlist) => %{((alt (seq ()) (seq x xlist)) "")},
      %(a | (b - c)) => %{((alt a (diff b c)) "")},
      %(a b | c d) => %{((alt (seq a b) (seq c d)) "")},
      %(a | b | c) => %{((alt a b c) "")},
      %{a) b c} => %{(a " b c")},
      %(BaseDecl? PrefixDecl*) => %{((seq (opt BaseDecl) (star PrefixDecl)) "")},
      %(NCCHAR1 | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]) =>
        %{((alt NCCHAR1 "-" (range "0-9") (hex "#x00B7") (range "#x0300-#x036F") (range "#x203F-#x2040")) "")}
    }.each do |input, expected|
      it "given #{input.inspect} produces #{expected}" do
        expect(ebnf(:expression, input).to_sxp).to produce(expected, @debug)
      end
    end
  end

  describe "#diff" do
    {
      %{'abc' def}               => %{("abc" " def")},
      %{[0-9]}                   => %{((range "0-9") "")},
      %{#x00B7}                   => %{((hex "#x00B7") "")},
      %{[#x0300-#x036F]}         => %{((range "#x0300-#x036F") "")},
      %{[^<>'{}|^`]-[#x00-#x20]} => %{((diff (range "^<>'{}|^`") (range "#x00-#x20")) "")},
      %{a b c}                   => %{(a " b c")},
      %{a? b c}                  => %{((opt a) " b c")},
      %{( [?*+] )?}              => %{((opt (range "?*+")) "")},
      %(a - b)                   => %{((diff a b) "")},
    }.each do |input, expected|
      it "given #{input.inspect} produces #{expected}" do
        expect(ebnf(:diff, input).to_sxp).to produce(expected, @debug)
      end
    end
  end
  
  def ebnf(method, value, options = {})
    @debug = []
    options = {:debug => @debug}.merge(options)
    EBNF::Base.new("", options).send(method, value)
  end
end
