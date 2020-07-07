# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Rule do
  let(:debug) {[]}
  let(:ebnf) {EBNF.parse(File.open(File.expand_path("../../etc/ebnf.ebnf", __FILE__)))}
  subject {EBNF::Rule.new(:rule, "0", [:seq, :foo])}

  describe ".from_sxp" do
    context "accepts valid variations" do
      {
        "ebnf[1]": [
          %{(rule ebnf "1" (star (alt declaration rule)))},
          EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]])
        ],
        "ebnf[1] parsed": [
          [:rule, :ebnf, "1", [:star, [:alt, :declaration, :rule]]],
          EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]], kind: :rule)
        ],
        "pass": [
          %{(pass _pass (plus (range "#x9#xA#xD#x20")))},
          EBNF::Rule.new(nil, nil, [:plus, [:range, "#x9#xA#xD#x20"]], kind: :pass)
        ],
        "terminal": [
          %{(terminal O_ENUM "17" (seq "[^" (plus CHAR) "]"))},
          EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"])
        ],
        "alt": [
          %{(rule alt (alt a b c))},
          EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c], kind: :rule)
        ],
        "diff": [
          %{(terminal R_CHAR "21" (diff CHAR "]"))},
          EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal)
        ],
        "istr": [
          %{(terminal nc (istr "foo"))},
          EBNF::Rule.new(:nc, nil, [:istr, "foo"], kind: :terminal)
        ],
        "not": [
          %{(rule _a_1 "n.1" (not op1))},
          EBNF::Rule.new(:_a_1, "n.1", [:not, :op1], kind: :rule)
        ],
        "opt": [
          %{(rule _diff_1 "7.1" (opt _diff_2))},
          EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2], kind: :rule)
        ],
        "plus": [
          %{(rule seq "6" (plus diff))},
          EBNF::Rule.new(:seq, "6", [:plus, :diff], kind: :rule)
        ],
        "rept": [
          %{(rule rept "6" (rept 1 "*" diff))},
          EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff])
        ],
        "rept m.n": [
          %{(rule rept "6" (rept 3 5 diff))},
          EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff])
        ],
        "seq": [
          %{(rule seq (seq a b c))},
          EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c], kind: :rule)
        ],
        "star": [
          %{(rule _alt_1 "5.1" (star _alt_2))},
          EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2], kind: :rule)
        ]
      }.each do |title, (sxp, expected)|
        it title do
          res = EBNF::Rule.from_sxp(sxp)
          expect(res).to eq expected
        end
      end
    end

    context "rejects invalid variations" do
      {
        "alt (empty)": %{(rule alt (alt))},
        "diff (empty)": %{(terminal R_CHAR "21" (diff))},
        "diff (one)": %{(terminal R_CHAR "21" (diff CHAR))},
        "diff (three)": %{(terminal R_CHAR "21" (diff CHAR "]" ","))},
        "hex (empty)": %{(terminal hex (hex))},
        "hex (two)": %{(terminal hex (hex #x01 #x02))},
        "istr (empty)": %{(terminal nc (istr))},
        "istr (two)": %{(terminal nc (istr "foo" "bar"))},
        "not (empty)": %{(rule _a_1 "n.1" (not))},
        "not (two)": %{(rule _a_1 "n.1" (not op1 op2))},
        "opt (empty)": %{(rule _diff_1 "7.1" (opt))},
        "plus (empty)": %{(rule seq "6" (plus))},
        "plus (two)": %{(rule seq "6" (plus diff extra))},
        "rept (empty)": %{(rule rept "6" (rept))},
        "rept (one)": %{(rule rept "6" (rept 1))},
        "rept (two)": %{(rule rept "6" (rept 1 "*"))},
        "rept (four)": %{(rule rept "6" (rept 1 "*" diff extra))},
        "rept (float min)": %{(rule rept "6" (rept 1.1 1 diff))},
        "rept (negative min)": %{(rule rept "6" (rept -1 1 diff))},
        "rept (float max)": %{(rule rept "6" (rept 1 1.1 diff))},
        "rept (negative max)": %{(rule rept "6" (rept 1 -1 diff))},
        "star (empty)": %{(rule _alt_1 "5.1" (star))},
        "star (two)": %{(rule _alt_1 "5.1" (star diff extra))},
        "not op": %{(rule _bad nil (_bad))}
      }.each do |title, (sxp, expected)|
        it title do
          expect {EBNF::Rule.from_sxp(sxp)}.to raise_error(ArgumentError)
        end
      end
    end
  end

  describe "#to_sxp" do
    {
      "ebnf[1]": [
        EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]]),
        %{(rule ebnf "1" (star (alt declaration rule)))},
      ],
      "pass": [
        EBNF::Rule.new(nil, nil, [:plus, [:range, "#x20\\t\\r\\n"]], kind: :pass),
        %{(pass _pass (plus (range "#x20\\\\t\\\\r\\\\n")))},
      ],
      "terminal": [
        EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"]),
        %{(terminal O_ENUM "17" (seq "[^" (plus CHAR) "]"))},
      ],
      "alt": [
        EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c], kind: :rule),
        %{(rule alt (alt a b c))},
      ],
      "diff": [
        EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal),
        %{(terminal R_CHAR "21" (diff CHAR "]"))},
      ],
      "istr": [
        EBNF::Rule.new(:nc, nil, [:istr, "foo"], kind: :terminal),
        %{(terminal nc (istr "foo"))},
      ],
      "not": [
        EBNF::Rule.new(:_a_1, "n.1", [:not, :op1], kind: :rule),
        %{(rule _a_1 "n.1" (not op1))},
      ],
      "opt": [
        EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2], kind: :rule),
        %{(rule _diff_1 "7.1" (opt _diff_2))},
      ],
      "plus": [
        EBNF::Rule.new(:seq, "6", [:plus, :diff], kind: :rule),
        %{(rule seq "6" (plus diff))},
      ],
      "rept": [
        EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff]),
        %{(rule rept "6" (rept 1 "*" diff))},
      ],
      "rept m.n": [
        EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff]),
        %{(rule rept "6" (rept 3 5 diff))},
      ],
      "seq": [
        EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c], kind: :rule),
        %{(rule seq (seq a b c))},
      ],
      "star": [
        EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2], kind: :rule),
        %{(rule _alt_1 "5.1" (star _alt_2))},
      ]
    }.each do |title, (rule, sxp)|
      it title do
        expect(rule.to_sxp).to eq sxp
      end
    end
  end

  describe "#to_ttl" do
    {
      "ebnf[1]": [
        EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]]),
        %{
        :ebnf rdfs:label "ebnf";
          dc:identifier "1";
          g:star
            [ g:alt (
              :declaration
              :rule
            ) ] .},
      ],
      "pass": [
        EBNF::Rule.new(nil, nil, [:plus, [:range, "#x20\\t\\r\\n"]], kind: :pass),
        %{
        :_pass rdfs:label "_pass";
          g:plus [ re:matches "[\\\\u0020\\\\t\\\\r\\\\n]" ] .},
      ],
      "terminal": [
        EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"]),
        %{
        :O_ENUM rdfs:label "O_ENUM";
          dc:identifier "17";
          re:seq ( "[^" [ re:plus :CHAR ] "]" ) .},
      ],
      "alt": [
        EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c], kind: :rule),
        %{
        :alt rdfs:label "alt";
          g:alt ( :a :b :c ) .},
      ],
      "diff": [
        EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal),
        %{
        :R_CHAR rdfs:label "R_CHAR";
          dc:identifier "21";
          re:diff ( :CHAR "]" ) .},
      ],
      "istr": [
        EBNF::Rule.new(:nc, nil, [:istr, "foo"], kind: :terminal),
        %{
        :nc rdfs:label "nc";
          re:matches "foo" .},
      ],
      "not": [
        EBNF::Rule.new(:_a_1, "n.1", [:not, :op1], kind: :rule),
        %{
        :_a_1 rdfs:label "_a_1";
          dc:identifier "n.1";
          g:not :op1 .},
      ],
      "opt": [
        EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2], kind: :rule),
        %{
        :_diff_1 rdfs:label "_diff_1";
          dc:identifier "7.1";
          g:opt :_diff_2 .},
      ],
      "plus": [
        EBNF::Rule.new(:seq, "6", [:plus, :diff], kind: :rule),
        %{
        :seq rdfs:label "seq";
          dc:identifier "6";
          g:plus :diff .},
      ],
      "rept": [
        EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff]),
        %{
        :rept rdfs:label "rept";
          dc:identifier "6";
          g:min 1;
          g:max "*";
          g:rept  :diff .},
      ],
      "rept m.n": [
        EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff]),
        %{
        :rept rdfs:label "rept";
          dc:identifier "6";
          g:min 3;
          g:max 5;
          g:rept :diff .},
      ],
      "seq": [
        EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c], kind: :rule),
        %{
        :seq rdfs:label "seq";
          g:seq ( :a :b :c ) .},
      ],
      "star": [
        EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2], kind: :rule),
        %{
        :_alt_1 rdfs:label "_alt_1";
          dc:identifier "5.1";
          g:star :_alt_2 .},
      ]
    }.each do |title, (rule, ttl)|
      it title do
        expect(rule.to_ttl.gsub(/\s+/m, " ")).to eq ttl.gsub(/\s+/m, " ")
      end
    end
  end

  describe "#to_ruby" do
    {
      "ebnf[1]": [
        EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]]),
        %{EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]])},
      ],
      "pass": [
        EBNF::Rule.new(nil, nil, [:plus, [:range, "#x20\\t\\r\\n"]], kind: :pass),
        %{EBNF::Rule.new(:_pass, nil, [:plus, [:range, \"#x20\\\\t\\\\r\\\\n\"]], kind: :pass)},
      ],
      "terminal": [
        EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"]),
        %{EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"], kind: :terminal)},
      ],
      "alt": [
        EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c], kind: :rule),
        %{EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c])},
      ],
      "diff": [
        EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal),
        %{EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal)},
      ],
      "not": [
        EBNF::Rule.new(:_a_1, "n.1", [:not, :op1], kind: :rule),
        %{EBNF::Rule.new(:_a_1, "n.1", [:not, :op1])},
      ],
      "opt": [
        EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2], kind: :rule),
        %{EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2])},
      ],
      "plus": [
        EBNF::Rule.new(:seq, "6", [:plus, :diff], kind: :rule),
        %{EBNF::Rule.new(:seq, "6", [:plus, :diff])},
      ],
      "rept": [
        EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff]),
        %{EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff])},
      ],
      "rept m.n": [
        EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff]),
        %{EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff])},
      ],
      "seq": [
        EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c], kind: :rule),
        %{EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c])},
      ],
      "star": [
        EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2], kind: :rule),
        %{EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2])},
      ]
    }.each do |title, (rule, ruby)|
      it title do
        expect(rule.to_ruby).to eq ruby
      end
    end
  end

  describe "#to_bnf" do
    {
      "no-rewrite" => [
        [:seq, :foo],
        [EBNF::Rule.new(:rule, "0", [:seq, :foo])]
      ],
      "embedded rule" => [
        [:seq, [:alt, :foo]],
        [EBNF::Rule.new(:rule, "0", [:seq, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :foo])]
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
      # Diff may be a Rule or a Terminal now.
      #"diff rule" => [
      #  [:diff, "a", "b"],
      #  [EBNF::Rule.new(:rule, "0", [:diff, "a", "b"])]
      #],
      "hex rule" => [
        [:hex, "#x00B7"],
        [EBNF::Rule.new(:rule, "0", [:hex, "#x00B7"], kind: :terminal)]
      ],
      "range rule" => [
        [:range, "a"],
        [EBNF::Rule.new(:rule, "0", [:range, "a"], kind: :terminal)]
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

    context "exceptions" do
      {
        "diff" => [:diff, "foo", "foobar"],
        "not" => [:not, "foo"],
        "rept" => [:rept, 1, 2, "foo"],
      }.each do |title, expr|
        it title do
          rule = EBNF::Rule.new(:rule, "0", expr)
          expect {rule.to_bnf}.to raise_error(RuntimeError)
        end
      end
    end
  end

  describe "#to_peg" do
    {
      "no-rewrite" => [
        [:seq, :foo],
        [EBNF::Rule.new(:rule, "0", [:seq, :foo])]
      ],
      "embedded rule" => [
        [:seq, [:alt, :foo]],
        [EBNF::Rule.new(:rule, "0", [:seq, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :foo])]
      ],
      "opt rule" => [
        [:opt, :foo],
        [EBNF::Rule.new(:rule, "0", [:opt, :foo])]
      ],
      "two opt rule" => [
        [:alt, [:opt, :foo], [:opt, :bar]],
        [EBNF::Rule.new(:rule, "0", [:alt, :_rule_1, :_rule_2]),
         EBNF::Rule.new(:_rule_1, "0.1", [:opt, :foo]),
         EBNF::Rule.new(:_rule_2, "0.2", [:opt, :bar])]
      ],
      "star rule" => [
        [:star, :foo],
        [EBNF::Rule.new(:rule, "0", [:star, :foo])]
      ],
      "plus rule" => [
        [:plus, :foo],
        [EBNF::Rule.new(:rule, "0", [:plus, :foo])]
      ],
      "diff rule" => [
        [:diff, "a", "b"],
        [EBNF::Rule.new(:rule, "0", [:seq, :_rule_1, "a"]),
         EBNF::Rule.new(:_rule_1, "0.1", [:not, "b"])]
      ],
      "hex rule" => [
        [:hex, "#x00B7"],
        [EBNF::Rule.new(:rule, "0", [:hex, "#x00B7"], kind: :terminal)]
      ],
      "range rule" => [
        [:range, "a"],
        [EBNF::Rule.new(:rule, "0", [:range, "a"], kind: :terminal)]
      ],
      "ebnf[1]" => [
        [:star, [:alt, :declaration, :rule]],
        [EBNF::Rule.new(:rule, "0", [:star, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:alt, :declaration, :rule])]
      ],
      "ebnf[9]" => [
        [:seq, :primary, [:opt, [:range, "?*+"]]],
        [EBNF::Rule.new(:rule, "0", [:seq, :primary, :_rule_1]),
         EBNF::Rule.new(:_rule_1, "0.1", [:opt, :_rule_2]),
         EBNF::Rule.new(:_rule_2, "0.2", [:range, "?*+"])]
      ],
      "IRIREF" => [
        [:seq, "<", [:star, [:alt, [:range, "^#x00-#x20<>\"{}|^`\\"], :UCHAR]], ">"],
        [EBNF::Rule.new(:rule, "0", [:seq, "<", :_rule_1, ">"]),
         EBNF::Rule.new(:_rule_1, "0.1", [:star, :_rule_2]),
         EBNF::Rule.new(:_rule_2, "0.2", [:alt, :_rule_3, :UCHAR]),
         EBNF::Rule.new(:_rule_3, "0.3", [:range, "^#x00-#x20<>\"{}|^`\\"])]
       ]
    }.each do |title, (expr, expected)|
      it title do
        rule = EBNF::Rule.new(:rule, "0", expr)
        expect(rule.to_peg).to eq expected
      end
    end

    it "extends with EBNF::PEG::Rule" do
      rule = EBNF::Rule.new(:rule, "0", [:seq, :foo]).to_peg.first
      expect(rule).to be_a(EBNF::PEG::Rule)
    end
  end

  describe "#to_regexp" do
    {
      hex: [:hex, "#x20", / /],
      range: [:range, "a-b", /[a-b]/],
      range2: [:range, "a-zA-Z", /[a-zA-Z]/],
      range3: [:range, "abc-", /[abc-]/],
    }.each do |title, (op, exp, regexp)|
      it title do
        expect(EBNF::Rule.new(title, nil, [op, exp]).to_regexp).to eql regexp
      end
    end

    {
      istr: ["foo", /foo/ui],
    }.each do |title, (exp, regexp)|
      it title, ruby: "!jruby" do
        expect(EBNF::Rule.new(title, nil, [title, exp]).to_regexp).to eql regexp
      end
    end

    it "raises an error for other operation" do
      expect {EBNF::Rule.new(:seq, nil, [:seq, :a]).to_regexp}.to raise_error(/Can't turn/)
    end
  end

  describe "#terminal?" do
    {
      "ebnf[1]": [
        EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]]),
        false,
      ],
      "pass": [
        EBNF::Rule.new(nil, nil, [:plus, [:range, "#x20\\t\\r\\n"]], kind: :pass),
        false,
      ],
      "terminal": [
        EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"]),
        true,
      ],
      "alt": [
        EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c], kind: :rule),
        false,
      ],
      "diff": [
        EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal),
        true,
      ],
      "istr": [
        EBNF::Rule.new(:nc, nil, [:istr, "foo"], kind: :terminal),
        true,
      ],
      "not": [
        EBNF::Rule.new(:_a_1, "n.1", [:not, :op1], kind: :rule),
        false,
      ],
      "opt": [
        EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2], kind: :rule),
        false,
      ],
      "plus": [
        EBNF::Rule.new(:seq, "6", [:plus, :diff], kind: :rule),
        false,
      ],
      "rept": [
        EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff]),
        false,
      ],
      "rept m.n": [
        EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff]),
        false,
      ],
      "seq": [
        EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c], kind: :rule),
        false,
      ],
      "star": [
        EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2], kind: :rule),
        false,
      ]
    }.each do |title, (rule, bool)|
      it "#{title} => #{bool.inspect}" do
        expect(rule.terminal?).to eq bool
      end
    end
  end

  describe "#pass?" do
    {
      "ebnf[1]": [
        EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]]),
        false,
      ],
      "pass": [
        EBNF::Rule.new(nil, nil, [:plus, [:range, "#x20\\t\\r\\n"]], kind: :pass),
        true,
      ],
      "terminal": [
        EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"]),
        false,
      ],
      "alt": [
        EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c], kind: :rule),
        false,
      ],
      "diff": [
        EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal),
        false,
      ],
      "istr": [
        EBNF::Rule.new(:nc, nil, [:istr, "foo"], kind: :terminal),
        false,
      ],
      "not": [
        EBNF::Rule.new(:_a_1, "n.1", [:not, :op1], kind: :rule),
        false,
      ],
      "opt": [
        EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2], kind: :rule),
        false,
      ],
      "plus": [
        EBNF::Rule.new(:seq, "6", [:plus, :diff], kind: :rule),
        false,
      ],
      "rept": [
        EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff]),
        false,
      ],
      "rept m.n": [
        EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff]),
        false,
      ],
      "seq": [
        EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c], kind: :rule),
        false,
      ],
      "star": [
        EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2], kind: :rule),
        false,
      ]
    }.each do |title, (rule, bool)|
      it "#{title} => #{bool.inspect}" do
        expect(rule.pass?).to eq bool
      end
    end
  end

  describe "#rule?" do
    {
      "ebnf[1]": [
        EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]]),
        true,
      ],
      "pass": [
        EBNF::Rule.new(nil, nil, [:plus, [:range, "#x20\\t\\r\\n"]], kind: :pass),
        false,
      ],
      "terminal": [
        EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"]),
        false,
      ],
      "alt": [
        EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c], kind: :rule),
        true,
      ],
      "diff": [
        EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal),
        false,
      ],
      "istr": [
        EBNF::Rule.new(:nc, nil, [:istr, "foo"], kind: :terminal),
        false,
      ],
      "not": [
        EBNF::Rule.new(:_a_1, "n.1", [:not, :op1], kind: :rule),
        true,
      ],
      "opt": [
        EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2], kind: :rule),
        true,
      ],
      "plus": [
        EBNF::Rule.new(:seq, "6", [:plus, :diff], kind: :rule),
        true,
      ],
      "rept": [
        EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff]),
        true,
      ],
      "rept m.n": [
        EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff]),
        true,
      ],
      "seq": [
        EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c], kind: :rule),
        true,
      ],
      "star": [
        EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2], kind: :rule),
        true,
      ]
    }.each do |title, (rule, bool)|
      it "#{title} => #{bool.inspect}" do
        expect(rule.rule?).to eq bool
      end
    end
  end

  describe "#alt?" do
    {
      "ebnf[1]": [
        EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]]),
        false,
      ],
      "pass": [
        EBNF::Rule.new(nil, nil, [:plus, [:range, "#x20\\t\\r\\n"]], kind: :pass),
        false,
      ],
      "terminal": [
        EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"]),
        false,
      ],
      "alt": [
        EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c], kind: :rule),
        true,
      ],
      "diff": [
        EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal),
        false,
      ],
      "istr": [
        EBNF::Rule.new(:nc, nil, [:istr, "foo"], kind: :terminal),
        false,
      ],
      "not": [
        EBNF::Rule.new(:_a_1, "n.1", [:not, :op1], kind: :rule),
        false,
      ],
      "opt": [
        EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2], kind: :rule),
        false,
      ],
      "plus": [
        EBNF::Rule.new(:seq, "6", [:plus, :diff], kind: :rule),
        false,
      ],
      "rept": [
        EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff]),
        false,
      ],
      "rept m.n": [
        EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff]),
        false,
      ],
      "seq": [
        EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c], kind: :rule),
        false,
      ],
      "star": [
        EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2], kind: :rule),
        false,
      ]
    }.each do |title, (rule, bool)|
      it "#{title} => #{bool.inspect}" do
        expect(rule.alt?).to eq bool
      end
    end
  end

  describe "#seq?" do
    {
      "ebnf[1]": [
        EBNF::Rule.new(:ebnf, "1", [:star, [:alt, :declaration, :rule]]),
        false,
      ],
      "pass": [
        EBNF::Rule.new(nil, nil, [:plus, [:range, "#x20\\t\\r\\n"]], kind: :pass),
        false,
      ],
      "terminal": [
        EBNF::Rule.new(:O_ENUM, "17", [:seq, "[^", [:plus, :CHAR], "]"]),
        true,
      ],
      "alt": [
        EBNF::Rule.new(:alt, nil, [:alt, :a, :b, :c], kind: :rule),
        false,
      ],
      "diff": [
        EBNF::Rule.new(:R_CHAR, "21", [:diff, :CHAR, "]"], kind: :terminal),
        false,
      ],
      "istr": [
        EBNF::Rule.new(:nc, nil, [:istr, "foo"], kind: :terminal),
        false,
      ],
      "not": [
        EBNF::Rule.new(:_a_1, "n.1", [:not, :op1], kind: :rule),
        false,
      ],
      "opt": [
        EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2], kind: :rule),
        false,
      ],
      "plus": [
        EBNF::Rule.new(:seq, "6", [:plus, :diff], kind: :rule),
        false,
      ],
      "rept": [
        EBNF::Rule.new(:rept, "6", [:rept, 1, "*", :diff]),
        false,
      ],
      "rept m.n": [
        EBNF::Rule.new(:rept, "6", [:rept, 3, 5, :diff]),
        false,
      ],
      "seq": [
        EBNF::Rule.new(:seq, nil, [:seq, :a, :b, :c], kind: :rule),
        true,
      ],
      "star": [
        EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2], kind: :rule),
        false,
      ]
    }.each do |title, (rule, bool)|
      it "#{title} => #{bool.inspect}" do
        expect(rule.seq?).to eq bool
      end
    end
  end

  describe "#==" do
    let(:rule1) {EBNF::Rule.new(:foo, nil, [:seq, "FOO"])}
    let(:rule2) {EBNF::Rule.new(:foo, nil, [:seq, "FOO"])}
    let(:rule3) {EBNF::Rule.new(:bar, nil, [:seq, "FOO"])}

    it "equals itself" do
      expect(rule1).to eq(rule1)
    end
    it "equals an equivalent rule" do
      expect(rule1).to eq(rule2)
    end
    it "does not equal a rule with a different symbol that has the same expression" do
      expect(rule1).not_to eq(rule3)
    end
  end

  describe "#eql?" do
    let(:rule1) {EBNF::Rule.new(:foo, nil, [:seq, "FOO"])}
    let(:rule2) {EBNF::Rule.new(:foo, nil, [:seq, "FOO"])}
    let(:rule3) {EBNF::Rule.new(:bar, nil, [:seq, "FOO"])}

    it "equals itself" do
      expect(rule1).to eql(rule1)
    end
    it "equals an equivalent rule" do
      expect(rule1).to eql(rule2)
    end
    it "equals a rule with a different symbol that has the same expression" do
      expect(rule1).to eql(rule3)
    end
  end

  describe "#translate_codepoints" do
    {
      "#x20" => " ",
      "#xffff" => "\u{ffff}"
    }.each do |str, cp|
      specify {expect(subject.translate_codepoints(str)).to eql(cp)}
    end
  end

  describe "#non_terminals" do
    subject {ebnf}
    {
      _pass: [],
      ebnf: [:declaration, :rule],
      declaration: [:pass],
      alt: [:seq],
      seq: [:diff],
      diff: [:postfix],
      postfix: [:primary],
      primary: [],
      pass: [],
      LHS: [],
      SYMBOL: [],
      HEX: [],
      ENUM: [],
      O_ENUM: [],
      RANGE: [],
      O_RANGE: [],
      STRING1: [],
      STRING2: [],
      CHAR: [],
      R_CHAR: [],
      POSTFIX: [],
      PASS: []
    }.each do |sym, expected|
      it "#{sym} => #{expected.inspect}" do
        res = subject.ast.find {|r| r.sym == sym}
        expect(res.non_terminals(subject.ast).map(&:sym)).to eq expected
      end
    end
  end

  describe "#terminals" do
    subject {ebnf}
    {
      _pass: [:PASS],
      ebnf: [],
      declaration: ["@terminals"],
      alt: [],
      seq: [],
      diff: [],
      postfix: [],
      primary: [:HEX, :SYMBOL, :ENUM, :O_ENUM, :RANGE, :O_RANGE, :STRING1, :STRING2, "("],
      pass: ["@pass"],
      LHS: ["["],
      SYMBOL: ["a-z", "A-Z", "0-9", "_", "."],
      HEX: ["#x"],
      ENUM: ["[", :LHS],
      O_ENUM: ["[^"],
      RANGE: ["["],
      O_RANGE: ["[^"],
      STRING1: ['"'],
      STRING2: ["'"],
      CHAR: ["#x9#xA#xD", "#x20-#xD7FF", "#xE000-#xFFFD", "#x10000-#x10FFFF"],
      R_CHAR: [:CHAR, "]", "-"],
      POSTFIX: ["?*+"],
      PASS: ["#x9#xA#xD#x20", "#", "#x", "//", "/*", "(*"]
    }.each do |sym, expected|
      it "#{sym} => #{expected.inspect}" do
        res = subject.ast.find {|r| r.sym == sym}
        expect(res.terminals(subject.ast).map {|r| r.is_a?(EBNF::Rule) ? r.sym : r}).to eq expected
      end
    end
  end

  describe "#symbols" do
    subject {ebnf}
    {
      _pass: [:PASS],
      ebnf: [:declaration, :rule],
      declaration: [:pass],
      alt: [:seq],
      seq: [:diff],
      diff: [:postfix],
      postfix: [:primary, :POSTFIX],
      primary: [:HEX, :SYMBOL, :ENUM, :O_ENUM, :RANGE, :O_RANGE, :STRING1, :STRING2, :expression],
      pass: [:expression],
      LHS: [:SYMBOL],
      SYMBOL: [],
      HEX: [],
      ENUM: [:R_CHAR, :HEX, :LHS],
      O_ENUM: [:R_CHAR, :HEX],
      RANGE: [:R_CHAR, :HEX],
      O_RANGE: [:R_CHAR, :HEX],
      STRING1: [:CHAR],
      STRING2: [:CHAR],
      CHAR: [],
      R_CHAR: [:CHAR],
      POSTFIX: [],
      PASS: []
    }.each do |sym, expected|
      it "#{sym} => #{expected.inspect}" do
        res = subject.ast.find {|r| r.sym == sym}
        expect(res.symbols).to eq expected
      end
    end
  end

  describe "#validate!" do
    let(:gram) {EBNF.parse("a ::= 'b'?")}
    subject {gram.ast.first}

    {
      "missing rule": [
        "a ::= b",
        /In rule a: No rule found for b/
      ],
      "illegal string": [
        %{a ::= "\u{01}"},
        /syntax error/
      ],
      "empty range": [
        "a ::= []",
        /syntax error/
      ],
      "mixed enum char and hex": [
        "a ::= [b#x20]",
        %(In rule a: Range must be of form  HEX+ or R_CHAR+: was "b#x20")
      ],
      "mixed enum char and hex (2)": [
        "a ::= [#x20z]",
        %(In rule a: Range must be of form  HEX+ or R_CHAR+: was "#x20z")
      ],
      "mixed range char and hex": [
        "a ::= [b-#x20]",
        /syntax error/
      ],
      "mixed range char and hex (2)": [
        "a ::= [#x20-b]",
        /syntax error/
      ],
      "incomplete range": [
        "a ::= [-b]",
        /syntax error/
      ],
      "extra range": [
        "a ::= [a-b-c]",
        /syntax error/
      ],
    }.each do |name, (rule, message)|
      it name do
        expect {EBNF.parse(rule, validate: true)}.to raise_error SyntaxError, message
      end
    end

    # Validate rules that can only be created through modification
    {
      "alt (empty)":          [:alt],
      "diff (empty)":         [:diff],
      "diff (one)":           [:diff, 'A'],
      "diff (three)":         [:diff, 'A', 'B', 'C'],
      "hex (empty)":          [:hex],
      "hex (two)":            [:hex, '#x01', '#x02'],
      "hex (string)":         [:hex, 'string'],
      "istr (empty)":         [:istr],
      "istr (two)":           [:istr, 'A', 'B'],
      "not (empty)":          [:not],
      "not (two)":            [:not, 'A', 'B'],
      "opt (empty)":          [:opt],
      "plus (empty)":         [:plus],
      "plus (two)":           [:plus, 'A', 'B'],
      "rept (empty)":         [:rept],
      "rept (one)":           [:rept, 1],
      "rept (two)":           [:rept, 1, 2],
      "rept (four)":          [:rept, 1, 2, 'A', 'B'],
      "rept (float min)":     [:rept, 1.1, 2, 'A'],
      "rept (negative min)":  [:rept, -1, 2, 'A'],
      "rept (float max)":     [:rept, 1, 2.1, 'A'],
      "rept (negative max)":  [:rept, 1, -1, 'A'],
      "star (empty)":         [:star],
      "star (two)":           [:star, 'A', 'B'],
      "not op":               [:bad]
    }.each do |title, expr|
      it title do
        subject.expr = expr
        expect {subject.validate!(gram.ast)}.to raise_error(SyntaxError)
      end
    end
  end

  describe "#valid?" do
    subject {EBNF.parse("a ::= b")}
    it "notes missing rule" do
      expect(subject.ast.first.valid?(subject.ast)).to be_falsey
    end

    it "validates EBNF" do
      ebnf = EBNF.parse(File.open(File.expand_path("../../etc/ebnf.ebnf", __FILE__)))
      expect(ebnf.ast.first).to be_valid(ebnf.ast)
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
end