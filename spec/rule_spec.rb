# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Rule do
  let(:debug) {[]}
  let(:ebnf) {EBNF.parse("", debug: debug)}
  subject {EBNF::Rule.new("rule", "0", [], ebnf: ebnf)}

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
        res.each {|r| expect(r).to be_a(String)}

        expect(res.join("\n").gsub(/\s+/, ' ')).to produce(expected, debug)
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
        expect(subject.send(:cclass, input)).to produce(expected, debug)
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
        [EBNF::Rule.new(:rule, "0", [:alt, :_empty, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:seq, :foo, :rule])]
      ],
      "plus rule" => [
        [:plus, :foo],
        [EBNF::Rule.new(:rule, "0", [:seq, :foo, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :_empty, :_rule_2]),
         EBNF::Rule.new(:_rule_2, "0.2", [:seq, :foo, :_rule_1])]
      ],
      "diff rule" => [
        [:diff, "a", "b"],
        [EBNF::Rule.new(:rule, "0", [:diff, "a", "b"], kind: :terminal)]
      ],
      "hex rule" => [
        [:hex, "#x00B7"],
        [EBNF::Rule.new(:rule, "0", [:hex, "#x00B7"], kind: :terminal)]
      ],
      "range rule" => [
        [:range, "a", "b"],
        [EBNF::Rule.new(:rule, "0", [:range, "a", "b"], kind: :terminal)]
      ],
      "ebnf[1]" => [
        [:star, [:alt, :declaration, :rule]],
        [EBNF::Rule.new(:rule, "0", [:alt, :_empty, :_rule_2]),
         EBNF::Rule.new(:_rule_2, "0.2", [:seq, :_rule_1, :rule]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :declaration, :rule])]
      ],
      "ebnf[9]" => [
        [:seq, :primary, [:opt, [:range, "?*+"]]],
        [EBNF::Rule.new(:rule, "0", [:seq, :primary, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :_empty, :_rule_2]),
         EBNF::Rule.new(:_rule_2, "0.2", [:range, "?*+"], kind: :terminal)]
      ],
      "IRIREF" => [
        [:seq, "<", [:star, [:alt, [:range, "^#x00-#x20<>\"{}|^`\\"], :UCHAR]], ">"],
        [EBNF::Rule.new(:rule, "0", [:seq, "<", :_rule_1, ">"]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :_empty, :_rule_3]),
         EBNF::Rule.new(:_rule_3, "0.3", [:seq, :_rule_2, :_rule_1]),
         EBNF::Rule.new(:_rule_2, "0.2", [:alt, :_rule_4, :UCHAR]),
         EBNF::Rule.new(:_rule_4, "0.4", [:range, "^#x00-#x20<>\"{}|^`\\"], kind: :terminal)]
       ]
    }.each do |title, (expr, expected)|
      it title do
        rule = EBNF::Rule.new(:rule, "0", expr)
        expect(rule.to_bnf).to eq expected
        case expr.first
        when :seq, :alt
          expect(rule).to be_starts_with(expr[1])
        else
          expect(rule).not_to be_starts_with(expr[1])
        end
      end
    end

    describe "#rewrite" do
      it "updates rule references" do
        subject.expr = [:do, :re, [:me, :fa, :do], :do]
        rsrc = EBNF::Rule.new(:do, "0", [])
        rdst = EBNF::Rule.new(:DO, "0", [])
        expect(subject.rewrite(rsrc, rdst).expr).to eq [:DO, :re, [:me, :fa, :do], :DO]
      end
    end
  end
end