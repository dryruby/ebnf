# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf'
require 'ebnf/terminals'
require 'ebnf/peg/parser'
require 'core'    # "Core" rules used in the ABNF spec.
require 'meta'    # "ABNF" rules used for parsing ABNF, itself
require 'scanf'
require 'logger'

class ABNFParser
  include EBNF::PEG::Parser

  # Regular expressions for both "Core" and ABNF-specific terminals.
  ALPHA = %r{[\x41-\x5A\x61-\x7A]}
  VCHAR = %r{[\x20-\x7E]}
  WSP = %r{[\x20\x09]}
  CRLF = %r{\x0D?\x0A}
  COMMENT = %r{;(?:#{WSP}|#{VCHAR})*#{CRLF}}
  C_NL = %r{#{COMMENT}|#{CRLF}}
  C_WSP = %r{#{WSP}|(?:#{C_NL}#{WSP})}

  ##
  # Hash of generated {EBNF::Rule} objects by symbol
  #
  # @return [Hash{Symbol => EBNF::Rule}]
  attr_reader :parsed_rules

  ##
  # The following ABNF grammar rules are treated as terminals.

  # `rulename     ::=  ALPHA (ALPHA | DIGIT | "-")*`
  terminal(:rulename, /#{ALPHA}(?:#{ALPHA}|[0-9-])*/) do |value|
    value.to_sym
  end

  # `defined_as   ::=  c_wsp* ("=" | "=/") c_wsp*`
  terminal(:defined_as, /#{C_WSP}*=\/?#{C_WSP}*/) {|value| value.strip}

  # `quoted_string::=  DQUOTE [#x20-#x21#x23-#x7E]* DQUOTE`
  terminal(:quoted_string, /"[\x20-\x21\x23-\x7E]*"/) do |value|
    value[1..-2]
  end

  # `bin_val      ::=  "b" BIT+ (("." BIT+)+ | ("-" BIT+))?`
  terminal(:bin_val, /b[01]+(?:(?:(?:\.[01]+)+)|(?:-[01]+))?/) do |value|
    if value.include?('.')
      # Interpret segments in binary creating a string
      value[1..-1].split('.').map {|b| b.to_i(base=2).chr}.join("")
    elsif value.include?('-')
      # Interpret as a range
      [:range, value[1..-1].split('-').map {|b| "#x%x" % b.to_i(base=2)}.join("-")]
    else
      # Interpret as a single HEX character
      [:hex, "#x%x" % value[1..-1].to_i(base=2)]
    end
  end

  # `dec_val      ::=  "d" DIGIT+ (("." DIGIT+)+ | ("-" DIGIT+))?`
  terminal(:dec_val, /d[0-9]+(?:(?:(?:\.[0-9]+)+)|(?:-[0-9]+))?/) do |value|
    if value.include?('.')
      # Interpret segments in decimal creating a string
      value[1..-1].split('.').map {|d| d.to_i.chr}.join("")
    elsif value.include?('-')
      # Interpret as a range
      [:range, value[1..-1].split('-').map {|d| "#x%x" % d.to_i}.join("-")]
    else
      # Interpret as a single HEX character
      [:hex, "#x%x" % value[1..-1].to_i]
    end
  end

  # `hex_val      ::=  "x" HEXDIG+  (("." HEXDIG+)+ | ("-" HEXDIG+))?`
  terminal(:hex_val, /x[0-9A-F]+(?:(?:(?:\.[0-9A-F]+)+)|(?:-[0-9A-F]+))?/) do |value|
    if value.include?('.')
      # Interpret segments in hexadecimal creating a string
      value[1..-1].split('.').map {|h| h.to_i(base=16).chr}.join("")
    elsif value.include?('-')
      # Interpret as a range
      [:range, value[1..-1].split('-').map {|h| "#x%x" % h.to_i(base=16)}.join("-")]
    else
      # Interpret as a single HEX character
      [:hex, "#x#{value[1..-1]}"]
    end
  end

  # `c_wsp        ::=  WSP | (c_nl WSP)`
  terminal(:c_wsp, C_WSP)

  # `c_nl         ::=  comment | CRLF`
  terminal(:c_nl, C_NL)

  # `DIGIT        ::=  [#x30-#x39]`
  terminal(:DIGIT, /\d/)

  # ## Non-terminal productions

  # The `start_production` on `:rule` allows the parser to present the value as a single Hash, rather than an array of individual hashes.
  start_production(:rule, as_hash: true)

  # `rule         ::=  rulename defined_as elements c_nl`
  production(:rule) do |value|
    # value contains an expression.
    # Invoke callback
    sym = value[:rulename]
    elements = value[:elements]

    if value[:defined_as] == "=/"
      # append to rule alternate
      rule = parsed_rules.fetch(sym) {raise "No existing rule found for #{sym}"}
      rule.expr = [:alt, rule.expr] unless rule.alt?
      if elements.first == :alt
        # append alternatives to rule
        rule.expr.concat(elements[1..-1])
      else
        # add elements as last alternative
        rule.expr.push(elements)
      end
    else
      # There shouldn't be an existing rule
      raise "Redefining rule #{sym}" if parsed_rules.has_key?(sym)
      parsed_rules[sym] = EBNF::Rule.new(sym.to_sym, nil, elements)
    end
    sym
  end

  # `elements     ::=  alternation c_wsp*`
  production(:elements) do |value|
    value.first[:alternation]
  end

  # `alternation  ::=  concatenation (c_wsp* "/" c_wsp* concatenation)*`
  production(:alternation) do |value|
    unless value.last[:_alternation_1].empty?
      [:alt, value.first[:concatenation]] + value.last[:_alternation_1]
    else
      value.first[:concatenation]
    end
  end

  # The `_aleteration_2` rule comes from the expanded PEG grammar and serves as an opportunity to custommize the values presented to the `aleteration` rule.
  production(:_alternation_2) do |value|
    if Array(value.last[:concatenation]).first == :alt
      value.last[:concatenation][1..-1]
    else
      [value.last[:concatenation]]
    end
    value.last[:concatenation]
  end

  # `concatenation::=  repetition (c_wsp+ repetition)*`
  production(:concatenation) do |value|
    unless value.last[:_concatenation_1].empty?
      [:seq, value.first[:repetition]] + value.last[:_concatenation_1]
    else
      value.first[:repetition]
    end
  end
  start_production(:_concatenation_2, as_hash: true)
  production(:_concatenation_2) do |value|
    value[:repetition]
  end

  # `repetition   ::=  repeat? element`
  production(:repetition) do |value|
    rept = value.first[:_repetition_1]
    elt = value.last[:element]
    case rept
    when [0, '*'] then [:star, elt]
    when [1, '*'] then [:plus, elt]
    when nil      then elt
    else
      [:rept, rept.first, rept.last, elt]
    end
  end

  # `repeat       ::=  DIGIT+ | (DIGIT* "*" DIGIT*)`
  production(:repeat) do |value|
    if value.is_a?(Integer)
      [value, value]
    else
      [value.first, value.last]
    end
  end
  start_production(:_repeat_1, as_hash: true)
  production(:_repeat_1) {|value| value.values}
  production(:_repeat_2) {|value| value.join("").to_i}
  production(:_repeat_3) {|value| value.join("").to_i}
  production(:_repeat_4) {|value| value.length > 0 ? value.join("").to_i : '*'}

  # `element      ::=  rulename | group | option | char_val | num_val | prose_val`
  production(:element) do |value|
    value
  end

  # `group        ::=  "(" c_wsp* alternation c_wsp* ")"`
  start_production(:group, as_hash: true)
  production(:group) do |value|
    value[:alternation]
  end

  # `option       ::=  "[" c_wsp* alternation c_wsp* "]"`
  start_production(:option, as_hash: true)
  production(:option) do |value|
    [:opt, value[:alternation]]
  end

  # `case_insensitive_string ::= "%i"? quoted_string`
  production(:case_insensitive_string) do |value|
    require 'byebug'; byebug if value.first.has_key?(:case_sensitive_string)
    [:istr, value.last[:quoted_string]]
  end

  # `case_sensitive_string ::= "%s" quoted_string`
  production(:case_sensitive_string) do |value|
    value.last[:quoted_string]
  end

  # `num_val      ::=  "%" (bin_val | dec_val | hex_val)`
  production(:num_val) do |value|
    value.last[:_num_val_1]
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

    @parsed_rules = {}

    # Parses into `@parsed_rules`
    parse(@input,
          :rulelist,        # Starting rule
          ABNFMeta::RULES,  # PEG rules
          whitespace: '',   # No implicit whitespace
          **options)
  end

  ##
  # The AST includes the parsed rules along with built-in rules for ABNF used within the parsed grammar.
  #
  # @return [Array<EBNF::Rule>]
  def ast
    # Add built-in rules for standard ABNF rules not 
    parsed_rules.values.map(&:symbols).flatten.uniq.each do |sym|
      rule = ABNFCore::RULES.detect {|r| r.sym == sym}
      parsed_rules[sym] ||= rule
    end

    parsed_rules.values
  end

  # Output formatted S-Expression of grammar
  def to_sxp
    require 'sxp' unless defined?(SXP)
    # Output rules as a formatted S-Expression
    SXP::Generator.string(ast.map(&:for_sxp))
  end
end
