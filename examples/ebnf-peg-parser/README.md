# EBNF Parser example

This example implements an [EBNF][] parser equivalent to the built-in parser. The proximate result is an Abstract [S-Expression][] composed of sub-rules which can be directly executed by the parser. Effectively, this is a re-implementation of {EBNF::Parser} itself.

## Parsing the Grammar

    require 'ebnf'

    ebnf = EBNFPegParser.new(File.open("../../etc/ebnf.ebnf"))

Output rules and terminals as [S-Expressions][S-Expression], [Turtle][] or [EBNF][]

    puts ebnf.to_sxp

This generates a [S-Expression][] form of the grammar suitable for use by {EBNF}.

    (
     (pass _pass (seq PASS))
     (rule ebnf "1" (star (alt declaration rule)))
     (rule declaration "2" (alt "@terminals" pass))
     (rule rule "3" (seq LHS expression))
     (rule expression "4" (seq alt))
     (rule alt "5" (seq seq (star (seq "|" seq))))
     (rule seq "6" (plus diff))
     (rule diff "7" (seq postfix (opt (seq "-" postfix))))
     (rule postfix "8" (seq primary (opt POSTFIX)))
     (rule primary "9"
      (alt HEX SYMBOL ENUM O_ENUM RANGE O_RANGE STRING1 STRING2 (seq "(" expression ")")))
     (rule pass "10" (seq "@pass" expression))
     (terminal LHS "11" (seq (opt (seq "[" SYMBOL "]" (plus " "))) SYMBOL (star " ") "::="))
     (terminal SYMBOL "12" (plus (alt (range "a-z") (range "A-Z") (range "0-9") "_" ".")))
     (terminal HEX "13" (seq "#x" (plus (alt (range "a-f") (range "A-F") (range "0-9")))))
     (terminal ENUM "14" (diff (alt (seq "[" (plus R_CHAR)) (seq (plus HEX) "]")) LHS))
     (terminal O_ENUM "15" (alt (seq "[^" (plus R_CHAR)) (seq (plus HEX) "]")))
     (terminal RANGE "16" (seq "[" (plus (alt (seq R_CHAR "-" R_CHAR) (seq HEX "-" HEX))) "]"))
     (terminal O_RANGE "17"
      (seq "[^" (plus (alt (seq R_CHAR "-" R_CHAR) (seq HEX "-" HEX))) "]"))
     (terminal STRING1 "18" (seq "\"" (star (diff CHAR "\"")) "\""))
     (terminal STRING2 "19" (seq "'" (star (diff CHAR "'")) "'"))
     (terminal CHAR "20"
      (alt
       (range "#x9#xA#xD")
       (range "#x20-#xD7FF")
       (range "#xE000-#xFFFD")
       (range "#x10000-#x10FFFF")) )
     (terminal R_CHAR "21" (diff CHAR "]"))
     (terminal POSTFIX "22" (range "?*+"))
     (terminal PASS "23"
      (plus
       (alt
        (range "#x00-#x20")
        (seq (alt (diff "#" "#x") "//") (star (range "^#x0A#x0Dx")))
        (seq "/*" (star (alt (opt (seq "*" (range "^/"))) (range "^*"))) "*/")
        (seq "(*" (star (alt (opt (seq "*" (range "^)"))) (range "^*"))) "*)")) )) )

This can then be used as input to {EBNF.parse} to transform [EBNF][] to [PEG][] for parsing examples of the grammar using {EBNF::PEG::Parser}.

    ebnf --input-format sxp --peg ebnf.sxp -o ebnf.peg.sxp

An example [S-Expression][] for rule `ebnf`, which is decomposed into sub-rules as follows:

     (rule ebnf "1" (star _ebnf_1))
     (rule _ebnf_1 "1.1" (alt declaration rule))

Note that sub-production `_ebnf_1` is created, could be useful for some productions when creating parser logic, as described in the example walkthrough below.

## Example Walkthrough

This example uses the EBNF grammar from {file:/etc/ebnf.ebnf} to generate {file:meta}, which includes the resulting `RULES` table, used by {file:parser} to implement a parser for the grammar.

The first step is defining regular expressions for terminals used within the grammar. For the EBNF grammar, this is done in {EBNF::Terminals}. Note that the parser can operate without terminal definitions, but this can greatly improve parser performance.

The {file:parser} is implemented using the {EBNFPegParser} class, which includes {EBNF::PEG::Parser}.

### Parser basics
The parser operates directly using the rules from the abstract syntax tree generated by turning the original [EBNF][] grammar using {EBNF::PEG#make_peg}. Tokens are derived from terminal rules defined in the grammar or contained inline through non-terminal rule definitions. Tokens are either strings, which must be matched exactly, or symbols, which identify a regular expression used to match the terminal and yield a token. The association between terminal symbols and their regular expressions along with processing rules to invoke when they are identified are described in [Terminal definitions](#Terminal_definitions).

The parser starts with the specified rule, `ebnf` in this case, and executes that rule, which is expected to completely parse the input file potentially leaving some whitespace.

Non-terminal rules have an expression using one of the following:

`seq`
: A sequence of rules or terminals. If any (other than `opt` or `star`) to not parse, the rule is terminated as unmatched.
`opt`
: An optional rule or terminal. It either results in the matching rule or returns `nil`.
`alt`
: A list of alternative rules, which are attempted in order. It terminates with the first matching rule, or is terminated as unmatched, if no such rule is found.
`plus`
: A sequence of one or more of the matching rule. If there is no such rule, it is terminated as unmatched; otherwise, the result is an array containing all matched input.
`star`
: A sequence of zero or more of the matching rule. It will always return an array.

The starting rule will typically be of the form `(star sub_rule)` which will attempt to parse that sub rule until the end of input.

If a rule matches, it enters a _production_, which may invoke a _start production before matching is attempted, and will call any _production_ either if matched, or unmatched. That _production_ may choose to evaluate the returned abstract syntax tree to simplify the result, or create some semantic representation of that value.

Due to the nature of [PEG][] parsers, the same rule may be attempted at the same input location many times; this is optimized by use of a [Packrat][] memoizing cache, which remembers the result of a previous successful evaluation and short-circuits further execution.

Processing continues by continuing to look for productions sequence and pushing those productions onto the stack. When a production is complete, any associated _production handler_ is invoked, after popping off the top of the `prod_data` stack. The just removed hash is passed as `current` to the _production handler_. This is typically where the work of the parser happens. See [Production definitions](#Production_definitions) for more information.

### Terminal definitions
The {file:parser} uses a DSL to specify `terminals` and `productions` associated with rules in the grammar. Each `terminal` specifies the rule name, associated regular expression, and a block which is invoked when the parser recognizes the terminal:

    terminal(:SYMBOL, SYMBOL) do |value, prod|
      value.to_sym
    end

In this terminal definition, the SYMBOL terminal is recognized using the `SYMBOL` regular expression from {EBNF::Terminals::SYMBOL}. When found, the value of the symbol returned for use by productions which include it.

### Production definitions
During parsing, when a non-terminal production is identified, it attempts to invoke an associated `start_production` block. Typically there is nothing to do at the start of a production, so these are often left out. However, at times, it is necessary to prepare the production stack with information. For example, consider the _start production_ for `_alt_1` (a sub-production of `alt`).

    start_production(:_alt_1) do |data, callback|
      data[:i_was_here] = true
    end

This is associated with the '|' part of the `alt` production.

    [5] alt         ::= seq ('|' seq)*

When this is invoked, we have already processed one `seq`, which provided as part of the input value of the `:alt` production. The result is to remove the `seq` data and append it to the `alt` data in `value[:alt]`.

    production(:_alt_1) do |value, data, callback|
      data[:i_was_here] == true
      value.map {|a1| a1.last[:seq]}.compact # Get rid of '|'
    end

The final result of `alt`, will then be the hash containing :alt and an array of data matching the `seq` sub-productions.

The `_alt_1` production comes from the PEG definition:

     (rule _alt_1 "5.1" (star _alt_2))
     (rule _alt_2 "5.2" (seq "|" seq))

On completion, the associated _production_ for `:alt` is invoked:

    production(:alt) do |value|
      if value.last[:_alt_1].length > 0
        [:alt, value.first[:seq]] + value.last[:_alt_1]
      else
        value.first[:seq]
      end
    end

Looking at the EBNF grammar itself, we can see that the first declaration is

    [1] ebnf        ::= (declaration | rule)*

This is reduced to the [PEG][] [S-Expression][] noted above:

    (rule ebnf "1" (star _ebnf_1))
    (rule _ebnf_1 "1.1" (alt declaration rule))

The `ebnf` production uses the `alt` operator. When matching the production itself we can see that it is either a `declaration` or a `rule`. In this case of this parser, the result of parsing EBNF is an Abstract Syntax Tree, but in other cases it may create something else. In the case of the [Turtle gem][], the parser generates _RDF Triples_. Because the parser uses a streaming scanner, a file of any length can be passed to the parser, which emits triples as sufficient processing completes.

[Ruby]:         https://ruby-lang.org/
[YARD]:         https://yardoc.org/
[YARD-GS]:      https://rubydoc.info/docs/yard/file/docs/GettingStarted.md
[PDD]:          https://lists.w3.org/Archives/Public/public-rdf-ruby/2010May/0013.html
[EBNF]:         https://www.w3.org/TR/REC-xml/#sec-notation
[EBNF doc]:     https://dryruby.github.io/ebnf
[Packrat]:      https://pdos.csail.mit.edu/~baford/packrat/thesis/
[PEG]:          https://en.wikipedia.org/wiki/Parsing_expression_grammar
[S-expression]: https://en.wikipedia.org/wiki/S-expression
[Turtle]:       https://www.w3.org/TR/2012/WD-turtle-20120710/
[Turtle gem]:   https://rubygems.org/gems/rdf-turtle
