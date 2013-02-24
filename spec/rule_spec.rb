# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Rule do
  let(:debug) {[]}
  let(:ebnf) {EBNF.parse("", :debug => debug)}
  subject {EBNF::Rule.new("rule", "0", [], :ebnf => ebnf)}

  describe "#ttl_expr" do
    {
      "ebnf[1]" => [
        [:star, [:alt, :declaration, :rule]],
        %{g:star [ g:alt ( :declaration :rule ) ] .}
      ],
      "ebnf[2]" => [
        [:alt, "@terminals", "@pass"],
        %{g:alt ( "@terminals" "@pass" ) .}
      ],
      "ebnf[5]" => [
        :alt,
        %{g:seq ( :alt ) .}
      ],
      "ebnf[9]" => [
        [:seq, :primary, [:opt, [:range, "?*+"]]],
        %{g:seq ( :primary [ g:opt [ re:matches "[?*+]" ] ] ) .}
      ],
      "IRIREF" => [
        [:seq, "<", [:star, [:alt, [:range, "^#x00-#x20<>\"{}|^`\\"], :UCHAR]], ">"],
        %{g:seq ( "<" [ g:star [ g:alt ( [ re:matches "[^\\\\u0000-\\\\u0020<>\\\"{}|^`\\\\]" ] :UCHAR ) ] ] ">" ) .}
      ]
    }.each do |title, (expr, expected)|
      it title do
        res = subject.send(:ttl_expr, expr, "g", 0, false)
        res.each {|r| r.should be_a(String)}
          
        res.
          join("\n").
          gsub(/\s+/, ' ').
          should produce(expected, debug)
      end
    end
  end
  
  describe "#cclass" do
    {
      "passes normal stuff" => [
        %{^<>'{}|^`},
        %{[^<>'{}|^`]}
      ],
      "turns regular hex range into unicode range" => [
        %{#x0300-#x036F},
        %{[\\u0300-\\u036F]}
      ],
      "turns short hex range into unicode range" => [
        %{#xC0-#xD6},
        %{[\\u00C0-\\u00D6]}
      ],
      "turns 3 char hex range into unicode range" => [
        %{#x370-#x37D},
        %{[\\u0370-\\u037D]}
      ],
      "turns long hex range into unicode range" => [
        %{#x000300-#x00036F},
        %{[\\U00000300-\\U0000036F]}
      ],
      "turns 5 char hex range into unicode range" => [
        %{#x00370-#x0037D},
        %{[\\U00000370-\\U0000037D]}
      ],
    }.each do |title, (input, expected)|
      it title do
        subject.send(:cclass, input).should produce(expected, debug)
      end
    end
  end

  describe "#to_bnf" do
    {
      "no-rewrite" => [
        [:seq],
        [EBNF::Rule.new(:rule, "0", [:seq])]
      ],
      "embedded rule" => [
        [:seq, [:alt]],
        [EBNF::Rule.new(:rule, "0", [:seq, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt])]
      ],
      "opt rule" => [
        [:opt, :foo],
        [EBNF::Rule.new(:rule, "0", [:alt, :_empty, :foo])]
      ],
      "two opt rule" => [
        [:alt, [:opt, :foo], [:opt, :bar]],
        [EBNF::Rule.new(:rule, "0", [:alt, :_rule_1, :_rule_2]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :_empty, :foo]),
         EBNF::Rule.new(:_rule_2, "0.2", [:alt, :_empty, :bar])]
      ],
      "star rule" => [
        [:star, :foo],
        [EBNF::Rule.new(:rule, "0", [:alt, :_empty, :_rule_star]),
         EBNF::Rule.new(:_rule_star, "0.1", [:seq, :foo, :rule])]
      ],
      "plus rule" => [
        [:plus, :foo],
        [EBNF::Rule.new(:rule, "0", [:seq, :foo, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :_empty, :__rule_1_star]),
         EBNF::Rule.new(:__rule_1_star, "0.1*", [:seq, :foo, :_rule_1])]
      ],
      "ebnf[1]" => [
        [:star, [:alt, :declaration, :rule]],
        [EBNF::Rule.new(:rule, "0", [:alt, :_empty, :_rule_star]),
         EBNF::Rule.new(:_rule_star, "0*", [:seq, :_rule_1, :rule]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :declaration, :rule])]
      ],
      "ebnf[9]" => [
        [:seq, :primary, [:opt, [:range, "?*+"]]],
        [EBNF::Rule.new(:rule, "0", [:seq, :primary, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :_empty, [:range, "?*+"]])]
      ],
      "IRIREF" => [
        [:seq, "<", [:star, [:alt, [:range, "^#x00-#x20<>\"{}|^`\\"], :UCHAR]], ">"],
        [EBNF::Rule.new(:rule, "0", [:seq, "<", :_rule_1, ">"]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :_empty, :__rule_1_star]),
         EBNF::Rule.new(:__rule_1_star, "0.1*", [:seq, :__rule_1_1, :_rule_1]),
         EBNF::Rule.new(:__rule_1_1, "0.1.1", [:alt, [:range, "^#x00-#x20<>\"{}|^`\\"], :UCHAR])]
       ]
    }.each do |title, (expr, expected)|
      it title do
        EBNF::Rule.new(:rule, "0", expr).to_bnf.should == expected
      end
    end

    describe "#rewrite" do
      it "updates rule references" do
        subject.expr = [:do, :re, [:me, :fa, :do], :do]
        rsrc = EBNF::Rule.new(:do, "0", [])
        rdst = EBNF::Rule.new(:DO, "0", [])
        subject.rewrite(rsrc, rdst).expr.should == [:DO, :re, [:me, :fa, :do], :DO]
      end
    end
  end
end