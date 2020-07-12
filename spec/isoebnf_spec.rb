# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'

describe EBNF::ISOEBNF do
  let(:logger) {RDF::Spec.logger}
  after(:each) do |example|
    puts logger.to_s if example.exception && !example.exception.is_a?(RSpec::Expectations::ExpectationNotMetError)
  end

  context "rule variations" do
    {
      "legal meta_identifier": [
        'rulename = "foo" ;',
        %{((rule rulename (seq "foo")))}
      ],
      "digits": [
        %{
          digit_excluding_zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
          digit                = "0" | digit_excluding_zero ;
        },
        %{((rule digit_excluding_zero (alt "1" "2" "3" "4" "5" "6" "7" "8" "9"))
           (rule digit (alt "0" digit_excluding_zero)))}
      ],
      "sequence of numbers": [
        %{
          twelve                          = "1", "2" ;
          two_hundred_one                 = "2", "0", "1" ;
          three_hundred_twelve            = "3", twelve ;
          twelve_thousand_two_hundred_one = twelve, two_hundred_one ;
        },
        %{((rule twelve (seq "1" "2"))
           (rule two_hundred_one (seq "2" "0" "1"))
           (rule three_hundred_twelve (seq "3" twelve))
           (rule twelve_thousand_two_hundred_one (seq twelve two_hundred_one)))}
      ],
      "natural number": [
        %{natural_number = digit_excluding_zero, { digit } ;},
        %{((rule natural_number (seq digit_excluding_zero (star digit))))}
      ],
      "integer": [
        %{integer = "0" | [ "-" ], natural_number ;},
        %{((rule integer (alt "0" (seq (opt "-") natural_number))))}
      ],
      "simple grammar": [
        %q{
          letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
                 | "H" | "I" | "J" | "K" | "L" | "M" | "N"
                 | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
                 | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
                 | "c" | "d" | "e" | "f" | "g" | "h" | "i"
                 | "j" | "k" | "l" | "m" | "n" | "o" | "p"
                 | "q" | "r" | "s" | "t" | "u" | "v" | "w"
                 | "x" | "y" | "z" ;
          digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
          symbol = "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">"
                 | "'" | '"' | "=" | "|" | "." | "," | ";" ;
          character = letter | digit | symbol | "_" ;
 
          identifier = letter , { letter | digit | "_" } ;
          terminal = "'" , character , { character } , "'" 
                   | '"' , character , { character } , '"' ;
 
          lhs = identifier ;
          rhs = identifier
               | terminal
               | "[" , rhs , "]"
               | "{" , rhs , "}"
               | "(" , rhs , ")"
               | rhs , "|" , rhs
               | rhs , "," , rhs ;

          rule = lhs , "=" , rhs , ";" ;
          grammar = { rule } ;
        },
        %q{((rule letter
            (alt "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
             "S" "T" "U" "V" "W" "X" "Y" "Z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
             "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" ))
           (rule digit (alt "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
           (rule symbol (alt "[" "]" "{" "}" "(" ")" "<" ">" "'" "\"" "=" "|" "." "," ";"))
           (rule character (alt letter digit symbol "_"))
           (rule identifier (seq letter (star (alt letter digit "_"))))
           (rule terminal
            (alt (seq "'" character (star character) "'") (seq "\"" character (star character) "\"")))
           (rule lhs (seq identifier))
           (rule rhs
            (alt identifier terminal
             (seq "[" rhs "]")
             (seq "{" rhs "}")
             (seq "(" rhs ")")
             (seq rhs "|" rhs)
             (seq rhs "," rhs)) )
           (rule rule (seq lhs "=" rhs ";"))
           (rule grammar (star rule)))}
      ],
      "pascal": [
        %q{
          (* a simple program syntax in EBNF − Wikipedia *)
          program = 'PROGRAM', white_space, identifier, white_space, 
                     'BEGIN', white_space, 
                     { assignment, ";", white_space }, 
                     'END.' ;
          identifier = alphabetic_character, { alphabetic_character | digit } ;
          number = [ "-" ], digit, { digit } ;
          string = '"' , { all_characters - '"' }, '"' ;
          assignment = identifier , ":=" , ( number | identifier | string ) ;
          alphabetic_character = "A" | "B" | "C" | "D" | "E" | "F" | "G"
                               | "H" | "I" | "J" | "K" | "L" | "M" | "N"
                               | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
                               | "V" | "W" | "X" | "Y" | "Z" ;
          digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
          white_space = ? white_space characters ? ;
          all_characters = ? all visible characters ? ;
        },
        %q{((rule program
            (seq "PROGRAM" white_space identifier white_space "BEGIN" white_space
             (star (seq assignment ";" white_space)) "END." ))
           (rule identifier (seq alphabetic_character (star (alt alphabetic_character digit))))
           (rule number (seq (opt "-") digit (star digit)))
           (rule string (seq "\"" (star (diff all_characters "\"")) "\""))
           (rule assignment (seq identifier ":=" (seq (alt number identifier string))))
           (rule alphabetic_character
            (alt "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
             "S" "T" "U" "V" "W" "X" "Y" "Z" ))
           (rule digit (alt "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
           (rule white_space (seq "? white_space characters ?"))
           (rule all_characters (seq "? all visible characters ?")))}
      ],
      "AA": [
        %{AA = "A";},
        %{((terminal AA (seq "A")))}
      ],
      "BB": [
        %{BB = 3 * AA, "B";},
        %{ ((terminal BB (seq (rept 3 3 AA) "B")))}
      ],
      "CC": [
        %{CC = 3 * [AA], "C";},
        %{((terminal CC (seq (rept 3 3 (opt AA)) "C")))}
      ],
      "DD": [
        %{DD = {AA}, "D";},
        %{((terminal DD (seq (star AA) "D")))}
      ],
      "EE": [
        %{EE = AA, {AA}, "E";},
        %{((terminal EE (seq AA (star AA) "E")))}
      ],
      "FF": [
        %{FF = 3 * AA, 3 * [AA], "F";},
        %{((terminal FF (seq (rept 3 3 AA) (rept 3 3 (opt AA)) "F")))}
      ],
      "GG": [
        %{GG = {3 * AA}, "G";},
        %{((terminal GG (seq (star (rept 3 3 AA)) "G")))}
      ],
      "space": [
        %{space = ? US-ASCII character 32 ?;},
        %{((rule space (seq "? US-ASCII character 32 ?")))} # XXX probably not
      ],
      "something": [
        %{something = foo, ( bar );},
        %{((rule something (seq foo (seq bar))))}
      ],
      "diff": [
        %{first_terminal_character    = terminal_character - "'" ;},
        %{((rule first_terminal_character (diff terminal_character "'")))},
      ],
    }.each do |title, (input, expect)|
      it title do
        input << "\n" unless input.end_with?("\n")
        expect(parse(input).to_sxp).to produce(expect, logger)
      end
    end
  end

  context "alternate terminal characters" do
    {
      "digits /": [
        %{
          digit_excluding_zero = "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" ;
          digit                = "0" / digit_excluding_zero ;
        },
        %{((rule digit_excluding_zero (alt "1" "2" "3" "4" "5" "6" "7" "8" "9"))
           (rule digit (alt "0" digit_excluding_zero)))}
      ],
      "digits !": [
        %{
          digit_excluding_zero = "1" ! "2" ! "3" ! "4" ! "5" ! "6" ! "7" ! "8" ! "9" ;
          digit                = "0" ! digit_excluding_zero ;
        },
        %{((rule digit_excluding_zero (alt "1" "2" "3" "4" "5" "6" "7" "8" "9"))
           (rule digit (alt "0" digit_excluding_zero)))}
      ],
      #"integer (/ /)": [
      #  %{integer = "0" | (/ "-" /), natural_number ;},
      #  %{((rule integer (alt "0" (seq (opt "-") natural_number))))}
      #],
      "natural number (: :)": [
        %{natural_number = digit_excluding_zero, (: digit :) ;},
        %{((rule natural_number (seq digit_excluding_zero (star digit))))}
      ],
      "legal meta_identifier .": [
        'rulename = "foo" .',
        %{((rule rulename (seq "foo")))}
      ],
    }.each do |title, (input, expect)|
      it title do
        input << "\n" unless input.end_with?("\n")
        expect(parse(input).to_sxp).to produce(expect, logger)
      end
    end
  end

  context "illegal syntax" do
    {
      "something": "something = foo ( bar );"
    }.each do |title, input|
      it title do
        expect {parse(input)}.to raise_error(SyntaxError)
      end
    end
  end

  it "parses ISO EBNF grammar" do
    gram = parse(File.open(File.expand_path("../../etc/iso-ebnf.isoebnf", __FILE__)))
    expect(gram).to be_valid
  end

  def parse(input, **options)
    @debug = []
    EBNF.parse(input, debug: @debug, format: :isoebnf, **options)
  end
end
