# EBNF

[EBNF][] parser and generic parser generator.

## Description
This is a [Ruby][] implementation of an [EBNF][] and [BNF][] parser and parser generator.
It parses [EBNF][] grammars to [BNF][], generates [First/Follow and Branch][] tables for
[LL(1)][] grammars, which can be used with the stream [Tokenizer][] and [LL(1) Parser][].

Of note in this implementation is that the tokenizer and parser are streaming, so that they can
process inputs of arbitrary size.

## Usage
### Parsing an LL(1) Grammar

    require 'ebnf'

    ebnf = EBNF.parse(File.open(./etc/ebnf.bnf))

Output rules and terminals as S-Expressions, Turtle or EBNF

    puts ebnf.to_sxp
    puts ebnf.to_ttl
    puts ebnf.to_ebnf

Transform EBNF to BNF (generates `alt` or `seq` from `plus`, `star` or `opt`)

    ebnf.make_bnf

Generate [First/Follow][] rules for BNF grammars

    ebnf.first_follow(start_tokens)

Generate Terminal, [First/Follow and Branch][] tables as Ruby for parsing grammars

    ebnf.to_ruby

### Creating terminal definitions and parser rules to parse generated grammars

The parser is initialized to callbacks invoked on entry and exit
to each `terminal` and `production`. A trivial parser loop can be described as follows:

    require 'ebnf/ll1/parser'
    require 'meta'

    class Parser
      include Meta

      terminal(:SYMBOL, /([a-z]|[A-Z]|[0-9]|_)+/) do |parser, prod, token, input|
        # Add data based on scanned token to input
        input[:symbol] = token.value
      end

      production(:rule) do |parser, phase, input, current, callback|
        # Process on start of production when phase == :start
        # Set state for entry into recursed rules through current

        # Process on end of production when phase == :finish
        # return results in input, retrieve results from recursed rules in current

        # Callback to parser loop with callback
      end

      def initialize(input)
        parser_options = {
          :branch => BRANCH,
          :first => FIRST,
          :follow => FOLLOW
        }
        parse(input, start_symbol, parser_options) do |context, *data|
          # Process calls from callback from productions

        rescue ArgumentError, RDF::LL1::Parser::Error => e
          progress("Parsing completed with errors:\n\t#{e.message}")
          raise RDF::ReaderError, e.message if validate?
        end

## EBNF Grammar
The [EBNF][] variant used here is based on [W3C][] [EBNF][] as defined in the
[XML 1.0 recommendation](http://www.w3.org/TR/REC-xml/), with minor extensions.

    /* An EBNF grammar for EBNF */
    [1] ebnf        ::= (declaration | rule)*

    [2] declaration ::= '@terminals' | '@pass'

    [3] rule        ::= lhs '::=' expression

    [4] lhs         ::= '[' (SYMBOL | '.')+ ']' SYMBOL

    [5] expression  ::= alt

    [6] alt         ::= seq ('|' seq)*

    [7] seq         ::= diff+

    [8] diff        ::= postfix ('-' postfix)*

    [9] postfix     ::= primary ( [?*+] )?

    [10] primary    ::= HEX
                    |   RANGE
                    |   ENUM 
                    |   O_RANGE
                    |   O_ENUM
                    |   STRING1
                    |   STRING2
                    |   '(' expression ')'

    @terminals

    [11] SYMBOL     ::= ([a-z] | [A-Z] | [0-9] | "_")+

    [12] HEX        ::= '#x' ([0-9] | [a-f] | [A-F])+

    [13] RANGE      ::= '[' CHAR '-' CHAR ']'

    [14] ENUM       ::= '[' CHAR+ ']'

    [15] O_RANGE    ::= '[^' CHAR '-' CHAR ']'

    [16] OENUM      ::= '[^' CHAR+ ']'

    [17] STRING1    ::= '"' (CHAR - '"')* '"'

    [18] STRING2    ::= "'" (CHAR - "'")* "'"

    [19] CHAR       ::= HEX
                    |   ('\\' [\\trn'"])
                    |   [^\t\r\n'"]

    @pass           ::= (
                          [#x20\t\r\n]
                        |  
                        )+

##  Acknowledgements
Much of this work, particularly the generic parser, is inspired by work originally done by
Tim Berners-Lee's Python [predictive parser](http://www.w3.org/2000/10/swap/grammar/predictiveParser.py).

The EBNF parser was inspired by Dan Connolly's
[EBNF to Turtle processor](http://www.w3.org/2000/10/swap/grammar/ebnf2turtle.py),
[EBNF to BNF Notation-3 rules](http://www.w3.org/2000/10/swap/grammar/ebnf2bnf.n3),
and [First Follow Notation-3 rules](http://www.w3.org/2000/10/swap/grammar/first_follow.n3). 


## Documentation
Full documentation available on [Rubydoc.info][EBNF doc].

## Future Work
* Detect FIRST/FOLLOW and left-recursion conflicts.
* Generate HTML output of parser results.
* Better LL(1) parser tests

## Author
* [Gregg Kellogg](http://github.com/gkellogg) - <http://greggkellogg.net/>

## Contributing
* Do your best to adhere to the existing coding conventions and idioms.
* Don't use hard tabs, and don't leave trailing whitespace on any line.
* Do document every method you add using [YARD][] annotations. Read the
  [tutorial][YARD-GS] or just look at the existing code for examples.
* Don't touch the `.gemspec`, `VERSION` or `AUTHORS` files. If you need to
  change them, do so on your private branch only.
* Do feel free to add yourself to the `CREDITS` file and the corresponding
  list in the the `README`. Alphabetical order applies.
* Do note that in order for us to merge any non-trivial changes (as a rule
  of thumb, additions larger than about 15 lines of code), we need an
  explicit [public domain dedication][PDD] on record from you.

## License
This is free and unencumbered public domain software. For more information,
see <http://unlicense.org/> or the accompanying {file:UNLICENSE} file.

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