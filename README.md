# EBNF 

[EBNF][] parser and generic parser generator.

[![Gem Version](https://badge.fury.io/rb/ebnf.png)](http://badge.fury.io/rb/ebnf)
[![Build Status](https://secure.travis-ci.org/gkellogg/ebnf.png?branch=master)](http://travis-ci.org/gkellogg/ebnf)
[![Coverage Status](https://coveralls.io/repos/gkellogg/ebnf/badge.svg)](https://coveralls.io/r/gkellogg/ebnf)
[![Dependency Status](https://gemnasium.com/gkellogg/ebnf.png)](https://gemnasium.com/gkellogg/ebnf)

## Description
This is a [Ruby][] implementation of an [EBNF][] and [BNF][] parser and parser generator. It parses [EBNF][] grammars to [BNF][], generates [First/Follow][] and Branch tables for [LL(1)][] grammars, which can be used with the stream [Tokenizer][] and [LL(1) Parser][].

As LL(1) grammars operate using `alt` and `seq` primitives, allowing for a match on alternative productions or a sequence of productions, generating a parser requires turning the EBNF rules into BNF:

* Transform `a ::= b?` into `a ::= _empty | b`
* Transform `a ::= b+` into `a ::= b b*`
* Transform `a ::= b*` into `a ::= _empty | (b a)`
* Transform `a ::= op1 (op2)` into two rules:
  ```
  a     ::= op1 _a_1
  _a_1_ ::= op2
  ```

Of note in this implementation is that the tokenizer and parser are streaming, so that they can process inputs of arbitrary size.

## Usage
### Parsing an LL(1) Grammar

    require 'ebnf'

    ebnf = EBNF.parse(File.open('./etc/ebnf.ebnf'))

Output rules and terminals as S-Expressions, Turtle, HTML or BNF

    puts ebnf.to_sxp
    puts ebnf.to_ttl
    puts ebnf.to_html
    puts ebnf.to_s

Transform EBNF to BNF (generates sub-productions using `alt` or `seq` from `plus`, `star` or `opt`)

    ebnf.make_bnf

Generate [First/Follow][] rules for BNF grammars

    ebnf.first_follow(:ebnf)

Generate Terminal, [First/Follow][], Cleanup and Branch tables as Ruby for parsing grammars

    ebnf.build_tables
    ebnf.to_ruby

Generate formatted grammar using HTML (requires [Haml][Haml] gem)

    ebnf.to_html

### Parser S-Expressions
Intermediate representations of the grammar may be serialized to Lisp-like S-Expressions. For example, the rule `[1] ebnf        ::= (declaration | rule)*` is serialized as `(rule ebnf "1" (star (alt declaration rule)))`.

Once the [LL(1)][] conversion is made, the [First/Follow][] table is generated, this rule expands as follows:

     (rule ebnf "1"
      (start #t)
      (first "@pass" "@terminals" LHS _eps)
      (follow _eof)
      (cleanup star)
      (alt _empty _ebnf_2))
     (rule _ebnf_1 "1.1"
      (first "@pass" "@terminals" LHS)
      (follow "@pass" "@terminals" LHS _eof)
      (alt declaration rule))
     (rule _ebnf_2 "1.2"
      (first "@pass" "@terminals" LHS)
      (follow _eof)
      (cleanup merge)
      (seq _ebnf_1 ebnf))
     (rule _ebnf_3 "1.3" (first "@pass" "@terminals" LHS _eps) (follow _eof) (seq ebnf))

### Creating terminal definitions and parser rules to parse generated grammars
The parser is initialized to callbacks invoked on entry and exit
to each `terminal` and `production`. A trivial parser loop can be described as follows:

    require 'ebnf/ll1/parser'
    require 'meta'

    class Parser
      include Meta

      terminal(:SYMBOL, /([a-z]|[A-Z]|[0-9]|_)+/) do |prod, token, input|
        # Add data based on scanned token to input
        input[:symbol] = token.value
      end

      start_production(:rule) do |input, current, callback|
        # Process on start of production
        # Set state for entry into recursed rules through current

        # Callback to parser loop with callback
      end

      production(:rule) do |input, current, callback|
        # Process on end of production
        # return results in input, retrieve results from recursed rules in current

        # Callback to parser loop with callback
      end

      def initialize(input)
        parser_options = {
          branch: BRANCH,
          first: FIRST,
          follow: FOLLOW,
          cleanup: CLEANUP
        }
        parse(input, start_symbol, parser_options) do |context, *data|
          # Process calls from callback from productions

        rescue ArgumentError, RDF::LL1::Parser::Error => e
          progress("Parsing completed with errors:\n\t#{e.message}")
          raise RDF::ReaderError, e.message if validate?
        end

### Branch Table
The Branch table is a hash mapping production rules to a hash relating terminals appearing in input to sequence of productions to follow when the corresponding input terminal is found. This allows either the `seq` primitive, where all terminals map to the same sequence of productions, or the `alt` primitive, where each terminal may map to a different production.

    BRANCH = {
      :alt => {
        "(" => [:seq, :_alt_1],
        :ENUM => [:seq, :_alt_1],
        :HEX => [:seq, :_alt_1],
        :O_ENUM => [:seq, :_alt_1],
        :O_RANGE => [:seq, :_alt_1],
        :RANGE => [:seq, :_alt_1],
        :STRING1 => [:seq, :_alt_1],
        :STRING2 => [:seq, :_alt_1],
        :SYMBOL => [:seq, :_alt_1],
      },
      ...
      :declaration => {
        "@pass" => [:pass],
        "@terminals" => ["@terminals"],
      },
      ...
    }

In this case the `alt` rule is `seq ('|' seq)*` can happen when any of the specified tokens appears on the input stream. The all cause the same token to be passed to the `seq` rule and follow with `_alt_1`, which handles the `('|' seq)*` portion of the rule, after the first sequence is matched.

The `declaration` rule is `@terminals' | pass` using the `alt` primitive determining the production to run based on the terminal appearing on the input stream. Eventually, a terminal production is found and the token is consumed.

### First/Follow Table
The [First/Follow][] table is a hash mapping production rules to the terminals that may proceed or follow the rule. For example:

    FIRST = {
      :alt => [
        :HEX,
        :SYMBOL,
        :ENUM,
        :O_ENUM,
        :RANGE,
        :O_RANGE,
        :STRING1,
        :STRING2,
        "("],
      ...
    }

### Terminals Table
This table is a simple list of the terminal productions found in the grammar. For example:

    TERMINALS = ["(", ")", "-",
      "@pass", "@terminals",
      :ENUM, :HEX, :LHS, :O_ENUM, :O_RANGE,:POSTFIX,
      :RANGE, :STRING1, :STRING2, :SYMBOL,"|"
    ].freeze

### Cleanup Table
This table identifies productions which used EBNF rules, which are transformed to BNF for actual parsing. This allows the parser, in some cases, to reproduce *star*, *plus*, and *opt* rule matches. For example:

    CLEANUP = {
      :_alt_1 => :star,
      :_alt_3 => :merge,
      :_diff_1 => :opt,
      :ebnf => :star,
      :_ebnf_2 => :merge,
      :_postfix_1 => :opt,
      :seq => :plus,
      :_seq_1 => :star,
      :_seq_2 => :merge,
    }.freeze

In this case the `ebnf` rule was `(declaration | rule)*`. As BNF does not support a star operator, this is decomposed into a set of rules using `alt` and `seq` primitives:

    ebnf    ::= _empty _ebnf_2
    _ebnf_1 ::= declaration | rule
    _ebnf_2 ::= _ebnf_1 ebnf
    _ebnf_3 ::= ebnf

The `_empty` production matches an empty string, so allows for now value. `_ebnf_2` matches `declaration | rule` (using the `alt` primitive) followed by `ebnf`, creating a sequence of zero or more `declaration` or `alt` members.

## EBNF Grammar
The [EBNF][] variant used here is based on [W3C](http://w3.org/) [EBNF][] (see {file:etc/ebnf.ebnf EBNF grammar}) as defined in the
[XML 1.0 recommendation](http://www.w3.org/TR/REC-xml/), with minor extensions:

* Comments include `\\` and `#` through end of line (other than hex character) and `/* ... */ (* ... *) which may cross lines`
* All rules **MAY** start with an identifier, contained within square brackets. For example `[1] rule`, where the value within the brackets is a symbol `([a-z] | [A-Z] | [0-9] | "_" | ".")+`
* `@terminals` causes following rules to be treated as terminals. Any terminal which are entirely upper-case are also treated as terminals
* `@pass` defines the expression used to detect whitespace, which is removed in processing.
* No support for `wfc` (well-formedness constraint) or `vc` (validity constraint).

Parsing this grammar yields an S-Expression version: {file:etc/ebnf.ll1.sxp}.

## Example parser
For an example parser built using this gem, see {file:examples/ebnf-parser/README EBNF Parser example}. This example creates a parser for the [EBNF][] grammar which generates the same Abstract Syntax Tree as the built-in parser in the gem.

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
* Better LL(1) parser tests
* Either generate [Packrat parser][Packrat] for a [Parsing Regular Expression Grammar][PEG], or integrate with [Treetop][] or similar.

## Author
* [Gregg Kellogg](http://github.com/gkellogg) - <http://greggkellogg.net/>

## Contributing
This repository uses [Git Flow](https://github.com/nvie/gitflow) to mange development and release activity. All submissions _must_ be on a feature branch based on the _develop_ branch to ease staging and integration.

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

A copy of the [Turtle EBNF][] and derived parser files are included in the repository, which are not covered under the UNLICENSE. These files are covered via the [W3C Document License](http://www.w3.org/Consortium/Legal/2002/copyright-documents-20021231).

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
[Turtle EBNF]:  http://dvcs.w3.org/hg/rdf/file/default/rdf-turtle/turtle.bnf
[Packrat]:      http://pdos.csail.mit.edu/~baford/packrat/thesis/
[PEG]:          http://en.wikipedia.org/wiki/Parsing_expression_grammar
[Treetop]:      http://rubygems.org/gems/treetop
[Haml]:         http://rubygems.org/gems/haml
