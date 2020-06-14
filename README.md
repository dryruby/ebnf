# EBNF 

[EBNF][] parser and generic parser generator.

[![Gem Version](https://badge.fury.io/rb/ebnf.png)](https://badge.fury.io/rb/ebnf)
[![Build Status](https://secure.travis-ci.org/dryruby/ebnf.png?branch=master)](https://travis-ci.org/dryruby/ebnf)
[![Coverage Status](https://coveralls.io/repos/dryruby/ebnf/badge.svg)](https://coveralls.io/r/dryruby/ebnf)

## Description
This is a [Ruby][] implementation of an [EBNF][] and [BNF][] parser and parser generator.

### LL(1) Parser
In one mode, it parses [EBNF][] grammars to [BNF][], generates [First/Follow][] and Branch tables for [LL(1)][] grammars, which can be used with the stream [Tokenizer][] and [LL(1) Parser][].

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

See {EBNF::LL1} and {EBNF::LL1::Parser} for further information.

### [PEG][]/[Packrat][] Parser
An additional Parsing Expression Grammar ([PEG][]) parser generator is also supported. This performs more minmal transformations on the parsed grammar to extract sub-productions, which allows each component of a rule to generate its own parsing event.

## Usage
### Parsing an EBNF Grammar

    require 'ebnf'

    ebnf = EBNF.parse(File.open('./etc/ebnf.ebnf'))

Output rules and terminals as S-Expressions, Turtle, HTML or BNF

    puts ebnf.to_sxp
    puts ebnf.to_ttl
    puts ebnf.to_html
    puts ebnf.to_s

Transform EBNF to PEG (generates sub-rules for embedded expressions)

    ebnf.make_peg

Transform EBNF to BNF (generates sub-rules using `alt` or `seq` from `plus`, `star` or `opt`)

    ebnf.make_bnf

Generate [First/Follow][] rules for BNF grammars (using "ebnf" as the starting production):

    ebnf.first_follow(:ebnf)

Generate Terminal, [First/Follow][], Cleanup and Branch tables as Ruby for parsing grammars:

    ebnf.build_tables
    ebnf.to_ruby

Generate formatted grammar using HTML (requires [Haml][Haml] gem):

    ebnf.to_html

### Parser debugging

Inevitably while implementing a parser for some specific grammar, a developer will need greater insight into the operation of the parser. While this can involve sorting through a tremendous amount of data, the parser can be provided a {Logger}[] instance which will output messages at varying levels of detail to document the state of the parser at any given point. Most useful is likely the `INFO` level of debugging, but even more detail is revealed using the `DEBUG` level. `WARN` and `ERROR` statements will typically also be provided as part of an exception if parsing fails, but can be shown in the context of other parsing state with appropriate indentation as part of the logger.

### Parser errors
On a parsing failure, and exception is raised with information that may be useful in determining the source of the error.

## EBNF Grammar
The [EBNF][] variant used here is based on [W3C](https://w3.org/) [EBNF][] (see {file:etc/ebnf.ebnf EBNF grammar}) as defined in the
[XML 1.0 recommendation](https://www.w3.org/TR/REC-xml/), with minor extensions:

The general form of a rule is:

    symbol ::= expression

which can also be proceeded by an optional number enclosed in square brackets to identify the rule number:

    [1] symbol ::= expression

Symbols are written with an initial capital letter if they are the start symbol of a regular language (terminals), otherwise with an initial lowercase letter (non-terminals). Literal strings are quoted.

Within the expression on the right-hand side of a rule, the following expressions are used to match strings of one or more characters:

<table>
  <tr><td><code>#xN</code></td>
    <td>where <code>N</code> is a hexadecimal integer, the expression matches the character whose number (code point) in ISO/IEC 10646 is <code>N</code>. The number of leading zeros in the <code>#xN</code> form is insignificant.</td></tr>
  <tr><td><code>[a-zA-Z], [#xN-#xN]</code>
    <td>matches any Char with a value in the range(s) indicated (inclusive).</td></tr>
  <tr><td><code>[abc], [#xN#xN#xN]</code></td>
    <td>matches any Char with a value among the characters enumerated. Enumerations and ranges can be mixed in one set of brackets.</td></tr>
  <tr><td><code>[^a-z], [^#xN-#xN]</code></td>
    <td>matches any Char with a value outside the range indicated.</td></tr>
  <tr><td><code>[^abc], [^#xN#xN#xN]</code></td>
    <td>matches any Char with a value not among the characters given. Enumerations and ranges of forbidden values can be mixed in one set of brackets.</td></tr>
  <tr><td><code>"string"</code></td>
    <td>matches a literal string matching that given inside the double quotes.</td></tr>
  <tr><td><code>'string'</code></td>
    <td>matches a literal string matching that given inside the single quotes.</td></tr>
  <tr><td><code>A (B | C)</code></td>
    <td><code>(B | C)</code> is treated as a unit and may be combined as described in this list.</td></tr>
  <tr><td><code>A?</code></td>
    <td>matches A or nothing; optional A.</td></tr>
  <tr><td><code>A B</code></td>
    <td>matches <code>A</code> followed by <code>B</code>. This operator has higher precedence than alternation; thus <code>A B | C D</code> is identical to <code>(A B) | (C D)</code>.</td></tr>
  <tr><td><code>A | B</code></td>
    <td>matches <code>A</code> or <code>B</code>.</td></tr>
  <tr><td><code>A - B</code></td>
    <td>matches any string that matches <code>A</code> but does not match <code>B</code>.</td></tr>
  <tr><td><code>A+</code></td>
    <td>matches one or more occurrences of <code>A</code>. Concatenation has higher precedence than alternation; thus <code>A+ | B+</code> is identical to <code>(A+) | (B+)</code>.</td></tr>
  <tr><td><code>A*</code></td>
    <td>matches zero or more occurrences of <code>A</code>. Concatenation has higher precedence than alternation; thus <code>A* | B*</code> is identical to <code>(A*) | (B*)</code>.</td></tr>
  <tr><td><code>@pass " "*</code></td>
    <td>Defines consumed whitespace in the document. Any whitespace found between non-terminal rules is consumed and ignored.</td></tr>
  <tr><td><code>@terminals</code></td>
    <td>Introduces terminal rules. All rules defined after this point are treated as terminals.</td></tr>
</table>

* Comments include `//` and `#` through end of line (other than hex character) and `/* ... */ (* ... *) which may cross lines`
* All rules **MAY** start with an identifier, contained within square brackets. For example `[1] rule`, where the value within the brackets is a symbol `([a-z] | [A-Z] | [0-9] | "_" | ".")+`
* `@terminals` causes following rules to be treated as terminals. Any terminal which is all upper-case (eg`TERMINAL`), or any rules with expressions that match characters (`#xN`, `[a-z]`, `[^a-z]`, `[abc]`, `[^abc]`, `"string"`, `'string'`, or `A - B`), are also treated as terminals.
* `@pass` defines the expression used to detect whitespace, which is removed in processing.
* No support for `wfc` (well-formedness constraint) or `vc` (validity constraint).

Parsing this grammar yields an S-Expression version: {file:etc/ebnf.sxp} (or [LL(1)][] version {file:etc/ebnf.ll1.sxp} or [PEG][] version {file:etc/ebnf.peg.sxp}).

### Parser S-Expressions
Intermediate representations of the grammar may be serialized to Lisp-like S-Expressions. For example, the rule

    [1] ebnf        ::= (declaration | rule)*

is serialized as

    (rule ebnf "1" (star (alt declaration rule)))

Different components of an EBNF rule expression are transformed into their own operator:

<table>
  <tr><td><code>#xN</code></td><td><code>(hex "#xN")</code></td></tr>
  <tr><td><code>[a-z#xN-#xN]</code></td><td><code>(range "a-z#xN-#xN")</code></td></tr>
  <tr><td><code>[abc#xN]</code></td><td><code>(range "abc#xN")</code></td></tr>
  <tr><td><code>[^a-z#xN-#xN]</code></td><td><code>(range "^a-z#xN-#xN")</code></td></tr>
  <tr><td><code>[^abc#xN]</code></td><td><code>(range "^abc#xN")</code></td></tr>
  <tr><td><code>"string"</code></td><td><code>"string"</code></td></tr>
  <tr><td><code>'string'</code></td><td><code>"string"</code></td></tr>
  <tr><td><code>A (B | C)</code></td><td><code>(seq (A (alt B C)))</code></td></tr>
  <tr><td><code>A?</code></td><td><code>(opt A)</code></td></tr>
  <tr><td><code>A B</code></td><td><code>(seq A B)</code></td></tr>
  <tr><td><code>A | B</code></td><td><code>(alt A B)</code></td></tr>
  <tr><td><code>A - B</code></td><td><code>(diff A B)</code></td></tr>
  <tr><td><code>A+</code></td><td><code>(plus A)</code></td></tr>
  <tr><td><code>A*</code></td><td><code>(star A)</code></td></tr>
  <tr><td><code>@pass " "*</code></td><td><code>(pass (star " "))</code></td></tr>
  <tr><td><code>@terminals</code></td><td></td></tr>
</table>

Additionally, rules defined with an UPPERCASE symbol are treated as terminals.

For an [LL(1)][] parser generator, the {EBNF::BNF.make_bnf} method can be used to transform the EBNF rule into a BNF rule.

    (rule ebnf "1" (alt _empty _ebnf_2))
    (rule _ebnf_1 "1.1" (alt declaration rule))
    (rule _ebnf_2 "1.2" (seq _ebnf_1 ebnf))
    (rule _ebnf_3 "1.3" (seq ebnf))

This allows [First/Follow][] and other tables used by a parser to parse examples of the associated grammar. For more, see {EBNF::LL1}.

For a [PEG][] parser generator, there is a simpler transformation that reduces rules containing sub-expressions (composed of `star`, `alt`, `seq` and similar expressions) and creates named rules to allow appropriate callbacks and for naming elements of the generating abstract syntax tree. The {EBNF::PEG.make_peg} method transforms the original rule into the following two rules:

    (rule ebnf "1" (star _ebnf_1))
    (rule _ebnf_1 "1.1" (alt declaration rule))

## Example parsers
For a [PEG][] parser for a simple grammar implementing a calculator see [Calc example](http://dryruby.github.io/ebnf/examples/calc/doc/calc.html

For an example parser built using this gem that parses the [EBNF][] grammar, see [EBNF PEG Parser example](http://dryruby.github.io/ebnf/examples/ebnf-peg-parser/doc/parser.html). This example creates a parser for the [EBNF][] grammar which generates the same Abstract Syntax Tree as the built-in parser in the gem.

There is also an
[EBNF LL(1) Parser example](http://dryruby.github.io/ebnf/examples/ebnf-peg-parser/doc/parser.html).

##  Acknowledgements
Much of this work, particularly the generic parser, is inspired by work originally done by
Tim Berners-Lee's Python [predictive parser](https://www.w3.org/2000/10/swap/grammar/predictiveParser.py).

The [LL(1)][] parser was inspired by Dan Connolly's
[EBNF to Turtle processor](https://www.w3.org/2000/10/swap/grammar/ebnf2turtle.py),
[EBNF to BNF Notation-3 rules](https://www.w3.org/2000/10/swap/grammar/ebnf2bnf.n3),
and [First Follow Notation-3 rules](https://www.w3.org/2000/10/swap/grammar/first_follow.n3). 

## Documentation
Full documentation available on [Rubydoc.info][EBNF doc].

## Future Work
* Better LL(1) parser tests

## Author
* [Gregg Kellogg](https://github.com/gkellogg) - <https://greggkellogg.net/>

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
see <https://unlicense.org/> or the accompanying {file:UNLICENSE} file.

A copy of the [Turtle EBNF][] and derived parser files are included in the repository, which are not covered under the UNLICENSE. These files are covered via the [W3C Document License](https://www.w3.org/Consortium/Legal/2002/copyright-documents-20021231).

[Ruby]:         https://ruby-lang.org/
[YARD]:         https://yardoc.org/
[YARD-GS]:      https://rubydoc.info/docs/yard/file/docs/GettingStarted.md
[PDD]:          https://lists.w3.org/Archives/Public/public-rdf-ruby/2010May/0013.html
[BNF]:          https://en.wikipedia.org/wiki/Backus–Naur_form
[EBNF]:         https://www.w3.org/TR/REC-xml/#sec-notation
[EBNF doc]:     https://rubydoc.info/github/dryruby/ebnf
[First/Follow]: https://en.wikipedia.org/wiki/LL_parser#Constructing_an_LL.281.29_parsing_table
[LL(1)]:        https://www.csd.uwo.ca/~moreno//CS447/Lectures/Syntax.html/node14.html
[LL(1) Parser]: https://en.wikipedia.org/wiki/LL_parser
[Logger]:       https://ruby-doc.org/stdlib-2.4.0/libdoc/logger/rdoc/Logger.html
[Tokenizer]:    https://en.wikipedia.org/wiki/Lexical_analysis#Tokenizer
[Turtle EBNF]:  https://dvcs.w3.org/hg/rdf/file/default/rdf-turtle/turtle.bnf
[Packrat]:      https://pdos.csail.mit.edu/~baford/packrat/thesis/
[PEG]:          https://en.wikipedia.org/wiki/Parsing_expression_grammar
[Treetop]:      https://rubygems.org/gems/treetop
[Haml]:         https://rubygems.org/gems/haml
