# EBNF Parser example

This example implements an [EBNF][] parser equivalent to the built-in parser. The proximate result is an Abstract S-Expression which can be used to generate parser tables input grammars. Effectively, this is a re-implementation of {EBNF::Parser} itself.

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