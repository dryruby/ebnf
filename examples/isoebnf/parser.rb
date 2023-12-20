# # EBNF Parser for EISO BNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf'
require 'ebnf/terminals'
require 'ebnf/peg/parser'
require 'meta'
require 'sxp'
require 'logger'

class ISOEBNFPegParser
  include EBNF::PEG::Parser

  # The base for terminal-character, which omits "'", '"', and '?'.
  # Could be more optimized, and one might quible
  # with the overly-strictly defined character set,
  # but it is correct.
  TERMINAL_CHARACTER_BASE = %r{
    [a-zA-Z0-9] | # letter | decimal digit
    ,           | # concatenate symbol
    =           | # defining symbol
    [\|\/!]     | # definition separator symbol
    \*\)        | # end comment symbol
    \)          | # end group symbol
    \]          | # end option symbol
    \}          | # end repeat symbol
    \-          | # except symbol
    #\'          | # first quote symbol
    \*          | # repetition symbol
    #\"          | # second quote symbol
    #\?          | # special sequence symbol
    \(\*        | # start comment symbol
    \(          | # start group symbol
    \[          | # start option symbol
    \{          | # start repeat symbol
    [;\.]       | # terminator symbol
    [:+_%@&$<>^\x20\x23\\`~]  # other character
  }x

  TERMINAL_CHARACTER         = %r{#{TERMINAL_CHARACTER_BASE}|['"\?]}
  FIRST_TERMINAL_CHARACTER   = %r{#{TERMINAL_CHARACTER_BASE}|["\?]}
  SECOND_TERMINAL_CHARACTER  = %r{#{TERMINAL_CHARACTER_BASE}|['\?]}
  SPECIAL_SEQUENCE_CHARACTER = %r{#{TERMINAL_CHARACTER_BASE}|['"]}

  # Abstract syntax tree from parse
  #
  # @return [Array<EBNF::Rule>]
  attr_reader :ast

  # `[14] integer           ::= decimal_digit+`
  terminal(:integer, /\d+/) do |value, prod|
    value.to_i
  end

  # `[15] meta_identifier             ::= letter meta_identifier_character*`
  terminal(:meta_identifier, /[a-zA-Z][a-zA-Z0-9_]*/) do |value|
    value.to_sym
  end

  # `[17] terminal_string             ::= ("'" first_terminal_character+ "'")`
  # `                                   | ('"' second_terminal_character+ '"')`
  terminal(:terminal_string, /(?:'#{FIRST_TERMINAL_CHARACTER}+')|(?:"#{SECOND_TERMINAL_CHARACTER}+")/x) do |value|
    value[1..-2].tap {|s| s.quote_style = (value.start_with?("'") ? :squote : :dquote) }
  end

  # `[20] special_sequence            ::= '?' special_sequence_character* '?'`
  terminal(:special_sequence, /\?#{SPECIAL_SEQUENCE_CHARACTER}+\?/)

  # `[22] terminal_character          ::= [a-zA-Z0-9]`
  # `                                   | [,=;*}#x2d?([{;]`
  # `                                   | '*)'`
  # `                                   | '(*'`
  # `                                   | ']'`
  # `                                   | other_character`
  terminal(:terminal_character, TERMINAL_CHARACTER)

  # `[25] empty                      ::= ''`
  terminal(:empty, //)

  # `[26] definition_separator_symbol ::= '|' | '/' | '!'`
  terminal(:definition_separator_symbol, /[\|\/!]/)

  # `[27] terminator_symbol           ::= ';' | '.'`
  terminal(:terminator_symbol, /[;\.]/)

  # `[28] start_option_symbol         ::= '['
  terminal(:start_option_symbol, /\[|(?:\(\/)/)

  # `[29] end_option_symbol           ::= ']'`
  terminal(:end_option_symbol, /\]/)

  # `[30] start_repeat_symbol         ::= '{' | '(:'`
  terminal(:start_repeat_symbol, /{|\(:/)

  # `[31] end_repeat_symbol           ::= '}' | ':)'`
  terminal(:end_repeat_symbol, /}|:\)/)

  # ## Non-terminal productions

  # `[2]  syntax_rule       ::= meta_identifier '=' definitions_list terminator_symbol`
  production(:syntax_rule, clear_packrat: true) do |value, data, callback|
    # value contains an expression.
    # Invoke callback
    sym = value[0][:meta_identifier]
    definitions_list = value[2][:definitions_list]
    callback.call(:rule, EBNF::Rule.new(sym.to_sym, nil, definitions_list))
    nil
  end

  # Setting `as_hash: true` in the start production makes the value of the form of a hash, rather than an array of hashes.
  #
  # `[3]  definitions_list  ::= single_definition (definition_separator_symbol definitions_list)*`
  start_production(:definitions_list, as_hash: true)
  production(:definitions_list) do |value|
    if value[:_definitions_list_1].length > 0
      [:alt, value[:single_definition]] + value[:_definitions_list_1]
    else
      value[:single_definition]
    end
  end
  production(:_definitions_list_1) do |value|
    Array(value.first)
  end
  start_production(:_definitions_list_2, as_hash: true)
  production(:_definitions_list_2) do |value|
    if Array(value[:definitions_list]).first == :alt
      value[:definitions_list][1..-1]
    else
      [value[:definitions_list]]
    end
  end

  # `[4]  single_definition ::= term (',' term)*`
  start_production(:single_definition, as_hash: true)
  production(:single_definition) do |value|
    if value[:_single_definition_1].length > 0
      [:seq, value[:term]] + value[:_single_definition_1]
    else
      value[:term]
    end
  end
  production(:_single_definition_1) do |value|
    value.map {|a1| a1.last[:term]}.compact # Get rid of '|'
  end

  # `[5]  term              ::= factor ('-' exception)?`
  start_production(:term, as_hash: true)
  production(:term) do |value|
    if value[:_term_1]
      [:diff, value[:factor], value[:_term_1]]
    else
      value[:factor]
    end
  end
  production(:_term_1) do |value|
    value.last[:exception] if value
  end

  # `[6]  exception         ::= factor`
  start_production(:exception, as_hash: true)
  production(:exception) do |value|
    value[:factor]
  end

  # `[7]  factor            ::= (integer '*')? primary`
  start_production(:factor, as_hash: true)
  production(:factor) do |value|
    if value[:_factor_1]
      [:rept, value[:_factor_1], value[:_factor_1], value[:primary]]
    else
      value[:primary]
    end
  end
  production(:_factor_2) do |value|
    value.first[:integer]
  end

  # `[9]  optional_sequence ::= start_option_symbol definitions_list end_option_symbol`
  production(:optional_sequence) do |value|
    [:opt, value[1][:definitions_list]]
  end

  # `[10] repeated_sequence ::= start_repeat_symbol definitions_list end_repeat_symbol`
  production(:repeated_sequence) do |value|
    [:star, value[1][:definitions_list]]
  end

  # `[11] grouped_sequence  ::= '(' definitions_list ')'`
  production(:grouped_sequence) do |value|
    [:seq, value[1][:definitions_list]]
  end

  # ## Parser invocation.
  # On start, yield ourselves if a block is given, otherwise, return this parser instance
  #
  # @param  [#read, #to_s]          input
  # @param  [Hash{Symbol => Object}] options
  # @option options [Boolean] :level
  #   Trace level. 0(debug), 1(info), 2(warn), 3(error).
  # @return [EBNFParser]
  def initialize(input, **options, &block)
    # If the `level` option is set, instantiate a logger for collecting trace information.
    if options.has_key?(:level)
      options[:logger] = Logger.new(STDERR)
      options[:logger].level = options[:level]
      options[:logger].formatter = lambda {|severity, datetime, progname, msg| "#{severity} #{msg}\n"}
    end

    # Read input, if necessary, which will be used in a Scanner.
    @input = input.respond_to?(:read) ? input.read : input.to_s

    parsing_terminals = false
    @ast = []
    parse(@input,
           :syntax,
           ISOEBNFMeta::RULES,
           whitespace: %r{([\x09-\x0d\x20]|(?:\(\*(?:(?:\*[^\)])|[^*])*\*\)))+},
           **options
    ) do |context, *data|
      rule = case context
      when :rule
        # A rule which has already been turned into a `Rule` object.
        rule = data.first
        rule.kind = :terminal if parsing_terminals
        rule
      end
      @ast << rule if rule
    end
    @ast
  end

  # Output formatted S-Expression of grammar
  #
  # @return [String]
  def to_sxp(**options)
    require 'sxp' unless defined?(SXP)
    # Output rules as a formatted S-Expression
    SXP::Generator.string(@ast.map(&:for_sxp))
  end
end
