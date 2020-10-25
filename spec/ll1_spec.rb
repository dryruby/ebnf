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
          parse(%([1] ebnf        ::= (declaration | rule)*), start: :ebnf)
        }
        let(:rule) {ebnf_doc.ast.detect {|r| r.sym == :ebnf}}
        it "should include rule" do
          expect(rule).not_to be_nil
        end

        context "start rule" do
          subject {rule}
          its(:start) {expect(rule.start).to be_truthy}
          its(:follow) {should include(:_eof)}
        end
      end

      context "with illegitimate start rule" do
        specify {
          expect {
            parse(%([1] ebnf        ::= (declaration | rule)*), start: :foo)
          }.to raise_error("No rule found for start symbol foo")
        }
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
          expect(sin).to produce(expected, @debug)
        end
      end
    end

    context "first" do
      {
        "alt (Fi.2.1)" => [
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
        "sparqlPrefix (Fi.2.1/2.2)" => [
          %{
            [29s] sparqlBase                      ::= SPARQL_BASE IRIREF
            [18] IRIREF                           ::=  '<' ("range" | UCHAR)* '>'
            [29t] SPARQL_BASE                     ::= [Bb][Aa][Ss][Ee]
          },
          %{(
            (rule _empty "0" (first _eps) (seq))
            (terminal IRIREF "18" (seq "<" (star (alt "range" UCHAR)) ">"))
            (rule sparqlBase "29s" (first SPARQL_BASE) (seq SPARQL_BASE IRIREF))
            (terminal SPARQL_BASE "29t" (seq (range "Bb") (range "Aa") (range "Ss") (range "Ee")))
            (rule _sparqlBase_1 "29s.1" (first IRIREF) (seq IRIREF)))
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
        "Query (FF.1/6)" => [
          %{
            [2] Query        ::= 'BASE'? 'SELECT'
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule Query "2" (first "BASE" "SELECT") (seq _Query_1 "SELECT"))
             (rule _Query_1 "2.1" (first "BASE" _eps) (follow "SELECT") (cleanup opt) (alt _empty "BASE"))
             (rule _Query_2 "2.2" (first "SELECT") (seq "SELECT")))
          }
        ],
        "turtleDoc (FF.2)" => [
          %{
            [1] turtleDoc                         ::= statement* 
            [2] statement                         ::= directive | triples "." 
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule turtleDoc "1" (start #t) (first _eps) (follow _eof) (cleanup star) (alt _empty _turtleDoc_1))
             (rule _turtleDoc_1 "1.1" (follow _eof) (cleanup merge) (seq statement turtleDoc))
             (rule _turtleDoc_2 "1.2" (first _eps) (follow _eof) (seq turtleDoc))
             (rule statement "2" (follow _eof) (alt directive _statement_1))
             (rule _statement_1 "2.1" (follow _eof) (seq triples "."))
             (rule _statement_2 "2.2" (first ".") (follow _eof) (seq ".")))
          }, :turtleDoc
        ],
        "SolutionModifier" => [
          %{
            [18]    SolutionModifier	  ::= _SolutionModifier_1 
            [18.1]  _SolutionModifier_1 ::= _empty | GroupClause
            [19]    GroupClause	        ::= 'GROUP'
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule SolutionModifier "18" (first "GROUP" _eps) (seq _SolutionModifier_1))
             (rule _SolutionModifier_1 "18.1" (first "GROUP" _eps) (alt _empty GroupClause))
             (rule GroupClause "19" (first "GROUP") (seq "GROUP")))
          }
        ],
        "GroupGraphPattern" => [
          %{
            [54]  	GroupGraphPattern	  ::=  	'{' "E"? '}'
          },
          %[
            ((rule _empty "0" (first _eps) (seq) )
             (rule GroupGraphPattern "54" (first "{") (seq "{" _GroupGraphPattern_1 "}"))
             (rule _GroupGraphPattern_1 "54.1" (first "E" _eps) (follow "}") (cleanup opt) (alt _empty "E"))
             (rule _GroupGraphPattern_2 "54.2" (first "E" "}") (seq _GroupGraphPattern_1 "}"))
             (rule _GroupGraphPattern_3 "54.3" (first "}") (seq "}")))
          ]
        ]
      }.each do |name, (input, expected, start)|
        it name do
          ebnf = parse(input, start: start)
          sin = ebnf.ast.sort.to_sxp
          expect(sin).to produce(expected, @debug)
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
          }, nil
        ],
        "blankNodePropertyList (FF.4)" => [
          %{
            [7] predicateObjectList               ::= verb objectList ( ";" ( verb objectList)? )*
            [14] blankNodePropertyList            ::= "[" predicateObjectList "]"
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule predicateObjectList "7" (follow "]")
               (seq verb objectList _predicateObjectList_1))
             (rule _predicateObjectList_1 "7.1" (first ";" _eps) (follow "]") (cleanup star)
               (alt _empty _predicateObjectList_3))
             (rule _predicateObjectList_2 "7.2" (first ";") (follow ";" "]")
               (seq ";" _predicateObjectList_4))
             (rule _predicateObjectList_3 "7.3" (first ";") (follow "]") (cleanup merge)
               (seq _predicateObjectList_2 _predicateObjectList_1 ))
             (rule _predicateObjectList_4 "7.4" (first _eps) (follow ";" "]") (cleanup opt)
               (alt _empty _predicateObjectList_5))
             (rule _predicateObjectList_5 "7.5" (follow ";" "]")
               (seq verb objectList))
             (rule _predicateObjectList_6 "7.6" (follow "]")
               (seq objectList _predicateObjectList_1))
             (rule _predicateObjectList_7 "7.7" (first ";" _eps) (follow "]")
               (seq _predicateObjectList_1))
             (rule _predicateObjectList_8 "7.8" (first _eps) (follow ";" "]")
               (seq _predicateObjectList_4))
             (rule _predicateObjectList_9 "7.9" (follow ";" "]") (seq objectList))
             (rule blankNodePropertyList "14" (start #t) (first "[") (follow _eof) (seq "[" predicateObjectList "]"))
             (rule _blankNodePropertyList_1 "14.1" (follow _eof) (seq predicateObjectList "]") )
             (rule _blankNodePropertyList_2 "14.2" (first "]") (follow _eof) (seq "]")))
          }, :blankNodePropertyList
        ],
        "collection (FF.7/8)" => [
          %{
            [15] collection                       ::= "(" object* ")"
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule collection "15" (first "(") (seq "(" _collection_1 ")"))
             (rule _collection_1 "15.1" (first _eps) (follow ")") (cleanup star) (alt _empty _collection_2))
             (rule _collection_2 "15.2" (follow ")") (cleanup merge) (seq object _collection_1))
             (rule _collection_3 "15.3" (first ")") (seq _collection_1 ")"))
             (rule _collection_4 "15.4" (first _eps) (follow ")") (seq _collection_1))
             (rule _collection_5 "15.5" (first ")") (seq ")")))
          }, nil
        ],
        "turtleDoc (FF.6)" => [
          %{
            [1] turtleDoc ::= statement* 
            [2] statement ::= directive | triples "." 
            [3] directive ::= 'BASE'
            [4] triples   ::= 'IRI'
          },
          %{
            ((rule _empty "0" (first _eps) (seq))
             (rule turtleDoc "1" (start #t) (first "BASE" "IRI" _eps) (follow _eof) (cleanup star)
              (alt _empty _turtleDoc_1))
             (rule _turtleDoc_1 "1.1" (first "BASE" "IRI") (follow _eof) (cleanup merge) (seq statement turtleDoc))
             (rule _turtleDoc_2 "1.2" (first "BASE" "IRI" _eps) (follow _eof) (seq turtleDoc))
             (rule statement "2" (first "BASE" "IRI") (follow "BASE" "IRI" _eof) (alt directive _statement_1))
             (rule _statement_1 "2.1" (first "IRI") (follow "BASE" "IRI" _eof) (seq triples "."))
             (rule _statement_2 "2.2" (first ".") (follow "BASE" "IRI" _eof) (seq "."))
             (rule directive "3" (first "BASE") (follow "BASE" "IRI" _eof) (seq "BASE"))
             (rule triples "4" (first "IRI") (follow ".") (seq "IRI")))
          }, :turtleDoc
        ]
      }.each do |name, (input, expected, start)|
        it name do
          ebnf = parse(input, start: start)
          sin = ebnf.ast.sort.to_sxp
          expect(sin).to produce(expected, @debug)
        end
      end
    end
  end

  shared_examples "#build_tables" do |source, start|
    let!(:ebnf) {
      ebnf = parse(source, start: start)
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
          expect(ebnf.find_rule(t)).not_to be_nil
        end
      end
      it "has strings otherwise" do
        expect(other.map(&:class).uniq).to eq [String]
      end
      it "has strings used in all rules" do
        rule_strings = ebnf.ast.
          select {|r| r.rule?}.
          map(&:expr).flatten.
          select {|t| t.is_a?(String)}.
          uniq
        expect(rule_strings).to include(*other)
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
            expect(r).not_to be_nil
            expect(r).to be_rule
          end
        end
        it "values should all be terminals" do
          subject.values.flatten.compact.each do |t|
            expect(ebnf.terminals).to include(t) unless [:_eps, :_eof].include?(t)
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
          expect(r).not_to be_nil
          expect(r).to be_rule
        end
      end
      it "values should all be Hash's whose keys are terminals" do
        values = subject.values
        expect(values.map(&:class).uniq).to eq [Hash]
        values.map(&:keys).flatten.uniq.each do |t|
          expect(ebnf.terminals).to include(t)
        end
      end
      it "values of terminal keys are symbols of rules or strings" do
        symbols = subject.values.map(&:values).flatten.uniq
        expect(symbols.map(&:class).uniq).to include(Symbol, String)
      end
    end

    describe "EBNF::Base#to_ruby" do
      it "writes tables to output" do
        output = StringIO.new
        ebnf.to_ruby(output)
        output.rewind
        expect(output.read).not_to be_empty
      end
    end
  end

  describe "#build_tables" do
    context "error reporting" do
      before(:each) {$stderr, @old_stderr = StringIO.new, $stderr}
      after(:each) {$stderr = @old_stderr}
      {
        "generated terminal" => [
          "[1] implicit_terminal ::= [a-z]*",
          %r{terminal _implicit_terminal_1 is automatically generated},
          :implicit_terminal
        ],
        "First/First Conflict" => [
          %(
            [1] s ::= e | e "a"
            [2] e ::= "b"?
          ),
          %r{First/First Conflict: .* is the condition for both _s_1 and e},
          :s
        ],
        "First/Follow Conflict" => [
          %(
            [1] s ::= a "a" "b"
            [2] a ::= "a"?
          ),
          %r{First/Follow Conflict: .* is both first and follow of a},
          :s
        ],
      }.each do |name, (input, expected, start)|
        it name do
          ebnf = parse(input, start: start)
          expect {
            ebnf.build_tables
            expect(false).to produce(true, @debug)
          }.to raise_error("Table creation failed with errors")
          expect(ebnf.errors.to_s).to match(expected)

          sio = StringIO.new
          ebnf.to_ruby(sio)
          sio.rewind
          expect(sio.read).to match(/Note, grammar has errors/)
        end
      end
    end
  end

  describe "EBNF Grammar" do
    it_behaves_like "#build_tables",
      File.read(File.expand_path("../../etc/ebnf.ebnf", __FILE__)),
      :ebnf
  end

  describe "Turtle Grammar" do
    it_behaves_like "#build_tables",
      File.read(File.expand_path("../../etc/turtle.ebnf", __FILE__)),
      :turtleDoc

    let!(:ebnf) {
      ebnf = parse(File.read(File.expand_path("../../etc/turtle.ebnf", __FILE__)), start: :turtleDoc)
      ebnf.build_tables
      ebnf
    }
    subject {ebnf}

    # Spot check some productions
    {
      turtleDoc: [
        ['@prefix', '@base', :IRIREF],
        [:_eof]
      ],
      _predicateObjectList_1: [
        [";", :_eps],
        [".", "]"]
      ]
    }.each do |nt, (first, follow)|
      context nt do
        subject {ebnf.find_rule(nt)}
        it {should_not be_nil}
        its(:first) {expect(subject.first & first).to include(*first)}
        its(:follow) {expect(subject.follow & follow).to include(*follow)}
      end
    end
  end

  def parse(value, **options)
    @debug = []
    options = {debug: @debug, format: :native}.merge(options)
    ebnf = EBNF::Base.new(value, **options)
    ebnf.make_bnf
    @debug.clear
    ebnf.first_follow(options[:start])
    ebnf
  end
end
