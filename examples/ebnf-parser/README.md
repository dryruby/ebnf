# EBNF Parser example

This example implements an [EBNF][] parser equivalent to the built-in parser. The proximate result is an Abstract S-Expression which can be used to generate parser tables input grammars. Effectively, this is a re-implementation of {EBNF::Parser} itself.

## Parsing an LL(1) Grammar

    require 'ebnf'

    ebnf = EBNFParser.new(File.open(../../etc/ebnf.bnf))

Output rules and terminals as S-Expressions, Turtle or EBNF

    puts ebnf.to_sxp

This generates a S-Expression form of the grammar suitable for use by {EBNF} for generating a BNF representation (avoiding `star`, `plus`, and `opt` expressions), LL(1) first/follow comprehensions and branch tables used for parsing input files based on the grammar.

    ((pass (seq PASS))
     (rule ebnf "1" (star (alt declaration rule)))
     (rule declaration "2" (alt "@terminals" pass))
     (rule rule "3" (seq LHS expression))
     (rule expression "4" (seq alt))
     (rule alt "5" (seq seq (star (seq "|" seq))))
     (rule seq "6" (plus diff))
     (rule diff "7" (seq postfix (opt (seq "-" postfix))))
     (rule postfix "8" (seq primary (opt POSTFIX)))
     (rule primary "9"
      (alt HEX SYMBOL RANGE ENUM O_RANGE O_ENUM STRING1 STRING2 (seq "(" expression ")")))
     (rule pass "10" (seq "@pass" expression))
     (terminal LHS "11" (seq ENUM SYMBOL "::="))
     (terminal SYMBOL "12" (plus (alt (range "a-z") (range "A-Z") (range "0-9") "_" ".")))
     (terminal HEX "13"
      (seq "#x"
       (alt (range "0-9") (range "a-f") (range "A-F"))
       (alt (range "0-9") (range "a-f") (range "A-F"))) )
     (terminal RANGE "14" (seq "[" CHAR "-" CHAR "]"))
     (terminal ENUM "15" (seq "[" (plus CHAR) "]"))
     (terminal O_RANGE "16" (seq "[^" CHAR "-" CHAR "]"))
     (terminal O_ENUM "17" (seq "[^" (plus CHAR) "]"))
     (terminal STRING1 "18" (seq "\"" (star (alt CHAR (range "\t'[]()-"))) "\""))
     (terminal STRING2 "19" (seq "'" (star (alt CHAR (range "\t\"[]()-"))) "'"))
     (terminal CHAR "20"
      (alt HEX (seq "\\" (range "\\trn'\"[]()-")) (range "^\t\r\n'\"[]()-")))
     (terminal POSTFIX "21" (range "?*+"))
     (terminal PASS "22"
      (plus
       (alt
        (range "#x20\t\r\n")
        (seq (alt "#" "//") (star (range "^\r\n")))
        (seq "/*" (star (alt (opt (seq "*" (range "^/"))) (range "^*"))) "*/")) )) )

This can then be used as input to {EBNF.parse} to transform EBNF to BNF, create LL(1) first/follow rules and/or generate parser tables for parsing examples of the grammar using {EBNF::LL1::Parser}.

    ebnf --input-format sxp --bnf output.sxp
    ebnf --input-format sxp --ll1 ebnf --format rb output.sxp

An example S-Expression for rule `ebnf`, which uses both `start` and `alt` operators is transformed to use just BNF `alt` and `seq` operators, and include `first` and `follow` sets is shown here:

    (rule ebnf "1"
     (start #t)
     (first "@pass" "@terminals" LHS _eps)
     (follow _eof)
     (alt _empty _ebnf_2))
    (rule _ebnf_1 "1.1"
     (first "@pass" "@terminals" LHS)
     (follow "@pass" "@terminals" LHS _eof)
     (alt declaration rule))
    (rule _ebnf_2 "1.2" (first "@pass" "@terminals" LHS) (follow _eof) (seq _ebnf_1 ebnf))
    (rule _ebnf_3 "1.3" (first "@pass" "@terminals" LHS _eps) (follow _eof) (seq ebnf))

Note that sub-productions `_ebnf_1` through `_ebnf_3` are created, could be useful for some productions when creating parser logic, as described in the example walkthrough below.

## Example Walkthrough

This example uses the EBNF grammar from {file:/etc/ebnf.ebnf} to generate {file:meta}, which include the resulting `BRANCH`, `FIRST`, `FOLLOW`, `TERMINALS` and `PASS` tables, used by {file:parser} to implement a parser for the grammar.

The first step is defining regular expressions for terminals used within the grammar. The table generation process in {EBNF::LL1#build_tables} is not yet capable of automatically generating regular expressions for terminal productions, so they must be defined by hand. For the EBNF grammar, this is done in {file:terminals}.

The {file:parser} is implemented using the {EBNFParser} class, which includes {EBNF::LL1::Parser}, {EBNFParserMeta} and {EBNFParserTerminals}.

### Parser basics
The parser operates using the `BRANCH`, `FIRST`, `FOLLOW`, `START`, and `PASS` definitions from {file:meta}. Basically, the starting production has identified a possible set of starting tokens and it branches to different non-terminal productions when it finds a matching token. Tokens are derived from terminal rules defined in the grammar or contained inline through non-terminal rule definitions. Tokens are either strings, which must be matched exactly, or symbols, which identify a regular expression used to match the terminal and yield a token. The association between terminal symbols and their regular expressions along with processing rules to invoke when they are identified are described in [Terminal definitions](#Terminal_definitions).

As mentioned, a production is found then the next token returned from the input matches a _first_ of the current production. If this is the case, then the `BRANCH` table will identify another production; this is pushed onto the _production stack_, along with an empty hash in the associated _production data stack_ (`prod_data`). If a _start handler_ is associated with the production, it is invoked with the top of `prod_data`, a fresh `data` hash, which will be pushed onto `prod_data` after the _start handler_ completes, and a reference to a callback specified when the parser was invoked. This is an opportunity to modify information at the top of `prod_data`, or to prepare information needed by downstream productions by initializing the new top of `prod_data`.

Processing continues by continuing to look for productions sequence and pushing those productions onto the stack. When a production is complete, any associated _production handler_ is invoked, after popping off the top of the `prod_data` stack. The just removed hash is passed as `current` to the _production handler_. This is typically where the work of the parser happens. See [Production definitions](#Production_definitions) for more information.

### Terminal definitions
The {file:parser} uses a DSL to specify `terminals` and `productions` associated with rules in the grammar. Each `terminal` specifies the rule name, associated regular expression, and a block which is invoked when the parser recognizes the terminal:

    terminal(:SYMBOL, SYMBOL) do |prod, token, input|
      input[:terminal] = token.value.to_sym
    end

In this terminal definition, the SYMBOL terminal is recognized using the `SYMBOL` regular expression from {EBNFParserTerminals::SYMBOL}. When found, the value of the symbol is added to the `input` stack for use by non-terminal productions which include it.

### Production definitions
During parsing, when a non-terminal production is identified, it attempts to invoke an associated `start_production` block. Typically there is nothing to do at the start of a production, so these are often left out. However, at times, it is necessary to prepare the production stack with information. For example, consider the _start production_ for `_alt_1` (a sub-production of `alt`).

    start_production(:_alt_1) do |input, current, callback|
      seq = Array(input[:seq])
      (input[:alt] = [:alt]) << (seq.length > 2 ? seq : seq.last)
      input.delete(:seq)
    end

The `_alt_1` production comes from the LL(1) definition:

    (rule _alt_1 "5.1"
     (first _eps "|")
     (follow ")" "@pass" "@terminals" LHS _eof)
     (alt _empty _alt_3))


This is associated with the '|' part of the `alt` production.

    [5] alt         ::= seq ('|' seq)*

When this is invoked, we have already processed one `seq`, which is placed on the `prod_data` stack, as `input[:seq]`. The result is to remove the `seq` data and append it to the `alt` data in `input[:alt]`. The final result of `alt`, will then be the hash containing :alt and an array of data matching the `seq` sub-productions. Looking at the EBNF grammar itself, we can see that the first declaration is

    [1] ebnf        ::= (declaration | rule)*

This is reduced to the LL(1) S-Expression noted above:

    (rule ebnf "1"
     (start #t)
     (first "@pass" "@terminals" LHS _eps)
     (follow _eof)
     (alt _empty _ebnf_2))
    (rule _ebnf_1 "1.1"
     (first "@pass" "@terminals" LHS)
     (follow "@pass" "@terminals" LHS _eof)
     (alt declaration rule))
    (rule _ebnf_2 "1.2" (first "@pass" "@terminals" LHS) (follow _eof) (seq _ebnf_1 ebnf))
    (rule _ebnf_3 "1.3" (first "@pass" "@terminals" LHS _eps) (follow _eof) (seq ebnf))

The `ebnf` production uses the `alt` operator. When matching the production itself we can see that it is either a `declaration` or a `rule`. In this case of this parser, the result of parsing EBNF is an Abstract Syntax Tree, but in other cases it may create something else. In the case of the [Turtle gem][], the parser generates _RDF Triples_. Because the parser uses a streaming lexer, a file of any length can be passed to the parser, which emits triples as sufficient processing completes.

[Ruby]:         http://ruby-lang.org/
[YARD]:         http://yardoc.org/
[YARD-GS]:      http://rubydoc.info/docs/yard/file/docs/GettingStarted.md
[PDD]:          http://lists.w3.org/Archives/Public/public-rdf-ruby/2010May/0013.html
[EBNF]:         http://www.w3.org/TR/REC-xml/#sec-notation
[EBNF doc]:     http://rubydoc.info/github/gkellogg/ebnf/master/frames
[First/Follow]: http://en.wikipedia.org/wiki/LL_parser#Constructing_an_LL.281.29_parsing_table
[LL(1)]:        http://www.csd.uwo.ca/~moreno//CS447/Lectures/Syntax.html/node14.html
[LL(1) Parser]: http://en.wikipedia.org/wiki/LL_parser
[Tokenizer]:    http://en.wikipedia.org/wiki/Lexical_analysis#Tokenizer
[Turtle gem]:   http://rubygems.org/gems/rdf-turtle
