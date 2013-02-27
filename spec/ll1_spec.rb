# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Base do
  describe "#first_follow" do
    context "start" do
      context "with legitimate start rule" do
        let!(:ebnf_doc) {
          parse(%([1] ebnf        ::= (declaration | rule)*), :start => :ebnf)
        }
        let(:rule) {ebnf_doc.ast.detect {|r| r.sym == :ebnf}}
        it "should include rule" do
          rule.should_not be_nil
        end

        context "start rule" do
          subject {rule}
          its(:start) {rule.start.should be_true}
          its(:follow) {should include(:_eof)}
        end
      end

      context "with illegitimate start rule" do
        specify {
          lambda {parse(%([1] ebnf        ::= (declaration | rule)*), :start => :foo)
        }.should raise_error("No rule found for start symbol foo")}
      end
    end

    context "comprehensions" do
      {
        "alt" => [
          %{[1] ebnf        ::= declaration | rule},
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule ebnf "1" (alt declaration rule)))
          }
        ],
        "seq[1]" => [
          %{[1] rule::= a b },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule rule "1" (seq a b))
             (rule _rule_1 "1.1" (seq b)))
          }
        ],
        "blankNodePropertyList" => [
          %{[14] blankNodePropertyList            ::= "[" predicateObjectList "]"},
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule blankNodePropertyList "14" (first "[") (seq "[" predicateObjectList "]"))
             (rule _blankNodePropertyList_1 "14.1" (seq predicateObjectList "]"))
             (rule _blankNodePropertyList_2 "14.2" (first "]") (seq "]")))
          }
        ]
      }.each do |name, (input, expected)|
        it name do
          ebnf = parse(input)
          sin = ebnf.ast.sort.to_sxp
          sin.should produce(expected, @debug)
        end
      end
    end

    context "first" do
      {
        "alt (FF.1)" => [
          %{
            [5] base                              ::= '@base' IRIREF "."
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule base "5" (first "@base") (seq "@base" IRIREF "."))
             (rule _base_1 "5.1" (seq IRIREF "."))
             (rule _base_2 "5.2" (first ".") (seq ".")))
          }, nil
        ],
        "sparqlPrefix (FF.1)" => [
          %{
            [29s] sparqlBase                      ::= SPARQL_BASE IRIREF
            [18] IRIREF                           ::=  '<' ("range" | UCHAR)* '>'
            [29t] SPARQL_BASE                     ::= [Bb][Aa][Ss][Ee]
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (terminal IRIREF "18" (seq "<" (star (alt "range" UCHAR)) ">"))
             (rule sparqlBase "29s" (first SPARQL_BASE) (seq SPARQL_BASE IRIREF))
             (rule _sparqlBase_1 "29s.1" (first IRIREF) (seq IRIREF))
             (terminal SPARQL_BASE "29t" (seq (range "Bb") (range "Aa") (range "Ss") (range "Ee"))))
          }, nil
        ],
        "declaration (FF.1)" => [
          %{
            [2] declaration ::= '@terminals' | '@pass'
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule declaration "2" (first "@pass" "@terminals") (alt "@terminals" "@pass")))
          }, nil
        ],
        "turtleDoc (FF.2)" => [
          %{
            [1] turtleDoc                         ::= statement* 
            [2] statement                         ::= directive | triples "." 
          },
          %{
            ((rule _empty "0" (first _eps) (follow _eof) (seq))
             (rule turtleDoc "1" (start #t) (first _eps) (follow _eof)
              (alt _empty _turtleDoc_1))
             (rule _turtleDoc_1 "1.1" (follow _eof) (seq statement turtleDoc))
             (rule _turtleDoc_2 "1.2" (first _eps) (follow _eof) (seq turtleDoc))
             (rule statement "2" (alt directive _statement_1))
             (rule _statement_1 "2.1" (seq triples "."))
             (rule _statement_2 "2.2" (first ".") (seq ".")))
          }, :turtleDoc
        ]
      }.each do |name, (input, expected, start)|
        it name do
          ebnf = parse(input, :start => start)
          sin = ebnf.ast.sort.to_sxp
          sin.should produce(expected, @debug)
        end
      end
    end

    context "follow" do
      {
        "objectList (FF.3)" => [
          %{
            [1] rule1 ::= a b
            [2] a     ::= "foo"
            [3] b     ::= "bar"
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule rule1 "1" (first "foo") (seq a b))
             (rule _rule1_1 "1.1" (first "bar") (seq b))
             (rule a "2" (first "foo") (follow "bar") (seq "foo"))
             (rule b "3" (first "bar") (seq "bar")))
          }
        ],
        "blankNodePropertyList (FF.4)" => [
          %{
            [7] predicateObjectList               ::= verb objectList
            [14] blankNodePropertyList            ::= "[" predicateObjectList "]"
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule predicateObjectList "7" (follow "]") (seq verb objectList))
             (rule _predicateObjectList_1 "7.1" (follow "]") (seq objectList))
             (rule blankNodePropertyList "14" (first "[") (seq "[" predicateObjectList "]"))
             (rule _blankNodePropertyList_1 "14.1" (seq predicateObjectList "]"))
             (rule _blankNodePropertyList_2 "14.2" (first "]") (seq "]")))
          }
        ],
        "collection (FF.6/7)" => [
          %{
            [15] collection                       ::= "(" object* ")"
          },
          %{
            ((rule _empty "0" (first _eps) (follow ")") (seq))
             (rule collection "15" (first "(") (seq "(" _collection_1 ")"))
             (rule _collection_1 "15.1" (first _eps) (follow ")") (alt _empty _collection_2))
             (rule _collection_2 "15.2" (follow ")") (seq object _collection_1))
             (rule _collection_3 "15.3" (first _eps) (seq _collection_1 ")"))
             (rule _collection_4 "15.4" (first _eps) (follow ")") (seq _collection_1))
             (rule _collection_5 "15.5" (first ")") (seq ")")))
          }
        ]
      }.each do |name, (input, expected)|
        it name do
          ebnf = parse(input)
          sin = ebnf.ast.sort.to_sxp
          sin.should produce(expected, @debug)
        end
      end
    end

  end

  describe "#build_tables" do
    context "EBNF Grammar" do
      let!(:ebnf) {
        ebnf = parse(File.read(File.expand_path("../../etc/ebnf.ebnf", __FILE__)), :start => :ebnf)
        ebnf.build_tables
        ebnf
      }
      subject {ebnf}
      context "#terminals" do
        subject {ebnf.terminals}
        let(:symbols) {subject.select {|t| t.is_a?(Symbol)}}
        let(:other) {subject.reject {|t| t.is_a?(Symbol)}}
        specify {should be_a(Array)}
        it "has symbols which are terminals" do
          symbols.each do |t|
            ebnf.find_rule(t).should_not be_nil
          end
        end
        it "includes all terminal rules (except CHAR)" do
          ebnf.ast.
            select {|r| r.kind == :terminal && r.sym != :CHAR}.
            map(&:sym).
            should =~ symbols
        end
        it "has strings otherwise" do
          other.map(&:class).uniq.should == [String]
        end
        it "has strings used in all rules" do
          rule_strings = ebnf.ast.
            select {|r| r.kind == :rule}.
            map(&:expr).flatten.
            select {|t| t.is_a?(String)}.
            uniq
          rule_strings.should =~ other
        end
      end

      [:first, :follow].each do |tab|
        context "#tab" do
          subject {ebnf.send(tab)}
          let(:symbols) {subject.select {|t| t.is_a?(Symbol)}}
          specify {should be_a(Hash)}
          it "keys are all rule symbols" do
            subject.keys.each do |sym|
              r = ebnf.find_rule(sym)
              r.should_not be_nil
              r.kind.should == :rule
            end
          end
          it "values should all be terminals" do
            subject.values.flatten.compact.each do |t|
              ebnf.terminals.should include(t) unless [:_eps, :_eof].include?(t)
            end
          end
        end
      end

      context "#branch" do
        subject {ebnf.branch}
        let(:symbols) {subject.select {|t| t.is_a?(Symbol)}}
        specify {should be_a(Hash)}
        it "keys are all rule symbols" do
          subject.keys.each do |sym|
            r = ebnf.find_rule(sym)
            r.should_not be_nil
            r.kind.should == :rule
          end
        end
        it "values should all be Hashs whos keys are terminals" do
          values = subject.values
          values.map(&:class).uniq.should == [Hash]
          values.map(&:keys).flatten.uniq.each do |t|
            ebnf.terminals.should include(t) unless [:_eps, :_eof, :_empty].include?(t)
          end
        end
        it "values of terminal keys are symbols of rules or strings" do
          symbols = subject.values.map(&:values).flatten.uniq
          symbols.map(&:class).uniq.should =~ [Symbol, String]
        end
      end
    end
  end

  def parse(value, options = {})
    @debug = []
    options = {:debug => @debug}.merge(options)
    ebnf = EBNF::Base.new(value, options)
    ebnf.make_bnf
    @debug.clear
    ebnf.first_follow(options[:start])
    ebnf
  end
end
