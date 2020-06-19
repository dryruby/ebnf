# Simple Calculator

This example is based on the
[Wikipedia entry on Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples). The parser calculates intermediate expressions and applies operations returning the value of the expression.

The Grammar is expressed as follows:

    [1] Expr    ::= Sum
    [2] Sum     ::= Product (('+' | '-') Product)*
    [3] Product ::= Power (('*' | '/') Power)*
    [4] Power   ::= Value ('^' Power)?
    [5] Value   ::= NUMBER | '(' Expr ')'
    [6] NUMBER  ::= [0-9]+

## Running the calculator

The calculator is expressed in `calc.rb` and can be exercised using the `calc` wrapper, but at its simplest can be invoked as follows:

    require 'calc'
    calc = Calc.new
    result = calc.evaluate('1 + 2 * 3')
    #=> 7

