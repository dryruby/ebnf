# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::Base do
  describe "#first_follow" do
    context "start" do
      let!(:doc) {parse(%([1] ebnf        ::= (declaration | rule)*), :debug => true)}
      before(:all) {doc.make_bnf}
      context "with legitimate start rule" do
        let!(:ebnf_doc) {
          d = doc.dup
          d.first_follow([:ebnf])
          d
        }
        subject {ebnf_doc.ast}
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
        subject {doc.dup}
        specify {lambda {subject.first_follow([:foo])}.should raise_error("No rule found for start symbol foo")}
      end
    end

    context "comprehensions" do
      {
        "alt" => [
          %{[1] ebnf        ::= declaration | rule},
          %{
            ((_empty "0" (kind rule) (first _eps) (seq))
             (ebnf "1" (kind rule) (alt declaration rule)))
          }
        ],
        "seq[1]" => [
          %{[1] rule::= a b },
          %{
            ((_empty "0" (kind rule) (first _eps) (seq))
             (rule "1" (kind rule) (seq a b))
             (_rule_comp "1.comp" (kind rule) (seq b)))
          }
        ],
        "blankNodePropertyList" => [
          %{[14] blankNodePropertyList            ::= "[" predicateObjectList "]"},
          %{
            ((_empty "0" (kind rule) (first _eps) (seq))
             (blankNodePropertyList "14" (kind rule) (first "[") (seq "[" predicateObjectList "]"))
             (_blankNodePropertyList_comp "14.comp" (kind rule) (seq predicateObjectList "]"))
             (__blankNodePropertyList_comp_comp "14.comp.comp" (kind rule) (first "]") (seq "]")))
          }
        ]
      }.each do |name, (input, expected)|
        context name do
          before(:each) {@debug = []}
          subject {
            c = parse(input, :debug => true)
            c.make_bnf
            c.first_follow([])
            c
          }
          it "produces #{expected}" do
            sin = subject.ast.to_sxp.gsub(/\s+/m, ' ')
            sout = expected.gsub(/\s+/m, ' ').strip
            sin.should produce(sout, @debug)
          end
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
            ((_empty "0" (kind rule) (first _eps) (seq))
             (base "5" (kind rule) (first "@base") (seq "@base" IRIREF "."))
             (_base_comp "5.comp" (kind rule) (seq IRIREF "."))
             (__base_comp_comp "5.comp.comp" (kind rule) (first ".") (seq ".")))
          }, []
        ],
        "sparqlPrefix (FF.1)" => [
          %{
            [29s] sparqlBase                      ::= SPARQL_BASE IRIREF
            [18] IRIREF                           ::=  '<' ("range" | UCHAR)* '>'
            [29t] SPARQL_BASE                     ::= [Bb][Aa][Ss][Ee]
          },
          %{
            ((_empty "0" (kind rule) (first _eps) (seq))
            (IRIREF "18" (kind terminal) (seq "<" (star (alt "range" UCHAR)) ">"))
            (sparqlBase "29s" (kind rule) (first SPARQL_BASE) (seq SPARQL_BASE IRIREF))
            (_sparqlBase_comp "29s.comp" (kind rule) (first IRIREF) (seq IRIREF))
            (SPARQL_BASE "29t" (kind terminal) (seq (range "Bb") (range "Aa") (range "Ss") (range "Ee"))))
          }, []
        ],
        "declaration (FF.1)" => [
          %{
            [2] declaration ::= '@terminals' | '@pass'
          },
          %{
            ((_empty "0" (kind rule) (first _eps) (seq))
            (declaration "2" (kind rule) (first "@pass" "@terminals") (alt "@terminals" "@pass")))
          }, []
        ],
        "turtleDoc (FF.2)" => [
          %{
            [1] turtleDoc                         ::= statement* 
            [2] statement                         ::= directive | triples "." 
          },
          %{
            ((_empty "0" (kind rule) (first _eps) (follow _eof) (seq))
             (turtleDoc "1" (kind rule) (start #t) (first _eps) (follow _eof)
              (alt _empty _turtleDoc_star))
             (_turtleDoc_star "1*" (kind rule) (follow _eof) (seq statement turtleDoc))
             (__turtleDoc_star_comp "1*.comp" (kind rule) (first _eps) (follow _eof) (seq turtleDoc))
             (statement "2" (kind rule) (alt directive _statement_1))
             (_statement_1 "2.1" (kind rule) (seq triples "."))
             (__statement_1_comp "2.1.comp" (kind rule) (first ".") (seq ".")))
          }, [:turtleDoc]
        ]
      }.each do |name, (input, expected, start)|
        context name do
          before(:each) {@debug = []}
          subject {
            c = parse(input)
            c.make_bnf
            @debug.clear
            c.first_follow(start)
            c
          }
          it "produces #{expected}" do
            sin = subject.ast.sort.to_sxp.gsub(/\s+/m, ' ')
            sout = expected.gsub(/\s+/m, ' ').strip
            sin.should produce(sout, @debug)
          end
        end
      end
    end

    context "follow" do
      {
        "objectList" => [
          %{
            [1] rule1 ::= a b
            [2] a     ::= "foo"
            [3] b     ::= "bar"
          },
          %{
            ((_empty "0" (kind rule) (first _eps) (seq))
             (rule1 "1" (kind rule) (first "foo") (seq a b))
             (_rule1_comp "1.comp" (kind rule) (first "bar") (seq b))
             (a "2" (kind rule) (first "foo") (follow "bar") (seq "foo"))
             (b "3" (kind rule) (first "bar") (seq "bar")))
          }, []
        ]
      }.each do |name, (input, expected, start)|
        context name do
          before(:each) {@debug = []}
          subject {
            c = parse(input)
            c.make_bnf
            @debug.clear
            c.first_follow(start)
            c
          }
          it "produces #{expected}" do
            sin = subject.ast.sort.to_sxp.gsub(/\s+/m, ' ')
            sout = expected.gsub(/\s+/m, ' ').strip
            sin.should produce(sout, @debug)
          end
        end
      end
    end

  end
  
  def parse(value, options = {})
    @debug = []
    options = {:debug => @debug}.merge(options)
    EBNF::Base.new(value, options)
  end
end
