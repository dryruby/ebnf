# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf/rule'
require 'ebnf/ll1/parser'
require_relative 'meta'
require_relative 'terminals'
require 'sxp'

class EBNFParser
  include EBNF::LL1::Parser
  include EBNFParserMeta
  include EBNFParserTerminals

  # Abstract syntax tree from parse
  #
  # @return [Array<EBNF::Rule>]
  attr_reader :ast

  # Grammar errors, or errors found genering parse tables
  #
  # @return [Array<String>]
  attr_accessor :errors

  # ## Terminals
  # Define rules for Terminals, placing results on the input stack, making them available to upstream non-Terminal rules.
  #
  # Terminals are matched in the order of appearance

  # Match the Left hand side of a rule or terminal
  #
  #     [11] LHS        ::= ENUM? SYMBOL "::="
  terminal(:LHS, LHS) do |prod, token, input|
    input[:id], input[:symbol] = token.value.to_s.scan(/\[([^\]]+)\]\s*(\w+)\s*::=/).first
  end

  # Match `SYMBOL` terminal
  #
  #     [12] SYMBOL     ::= ([a-z] | [A-Z] | [0-9] | "_" | ".")+
  terminal(:SYMBOL, SYMBOL) do |prod, token, input|
    input[:terminal] = token.value.to_sym
  end

  # Terminal for `RANGE` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [14] `RANGE`      ::= '[' CHAR '-' CHAR ']'
  terminal(:RANGE, RANGE, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Terminal for `ENUM` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [15] ENUM       ::= '[' CHAR+ ']'
  terminal(:ENUM, ENUM, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Terminal for `O_RANGE` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [16] O_RANGE    ::= '[^' CHAR '-' CHAR ']'
  terminal(:O_RANGE, O_RANGE, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Terminal for `O_ENUM` is matched as part of a `primary` rule. Unescape the values to remove EBNF escapes in the input.
  #
  #     [17] O_ENUM     ::= '[^' CHAR+ ']'
  terminal(:O_ENUM, O_ENUM, unescape: true) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Strings have internal escape sequences expanded and are passed through without surrounding quotes as terminals

  # Match double quote string
  #
  #     [18] STRING1    ::= '"' (CHAR | [\t\'\[\]\(\)\-])* '"'
  terminal(:STRING1, STRING1, unescape: true) do |prod, token, input|
    input[:terminal] = token.value[1..-2]
  end

  # Match single quote string
  #
  #     [19] STRING2    ::= "'" (CHAR | [\t\"\[\]\(\)\-])* "'"
  terminal(:STRING2, STRING2, unescape: true) do |prod, token, input|
    input[:terminal] = token.value[1..-2]
  end

  # Match `POSTFIX` terminal
  #
  #     [21] POSTFIX    ::= [?*+]
  terminal(:POSTFIX, POSTFIX) do |prod, token, input|
    input[:postfix] = token.value
  end

  # Make sure we recognize string terminals, even though they're not actually used in processing
  terminal(nil,                  %r(@terminals|@pass|[\[\]|\-\(\)])) do |prod, token, input|
    input[:terminal] = token.value
  end

  # ## Non-terminal productions
  # Define productions for non-Termainals. This can include `start_production` as well as `production` to hook into rule start and end. In some cases, we need to use sub-productions as generated when turning EBNF into BNF.

  # Production for end of `declaration` non-terminal.
  #
  # Look for `@terminals` to change parser state to parsing terminals.
  #
  # `@pass` is ignored here.
  #
  #     [2] declaration ::= '@terminals' | pass
  production(:declaration) do |input, current, callback|
    # current contains a declaration.
    # Invoke callback
    callback.call(:terminal) if current[:terminal] == '@terminals'
  end

  # Production for end of `rule` non-terminal.
  # The `input` parameter includes information placed by previous productions at the same level, or at the start of the current production.
  # The `current` parameter, is the result of child productions placing information onto their input.
  # The `callback` parameter provides access to a callback defined in the call to `parse`, see `#each_rule` below).
  #
  # Create rule from expression value and pass to callback
  #
  #     [3] rule        ::= LHS expression
  production(:rule) do |input, current, callback|
    # current contains an expression.
    # Invoke callback
    callback.call(:rule, EBNF::Rule.new(current[:symbol].to_sym, current[:id], current[:expression].last))
  end

  # Production for end of `expression` non-terminal.
  # Passes through the optimized value of the alt production as follows:
  #
  #     [:alt foo] => foo
  #     [:alt foo bar] => [:alt foo bar]
  #
  #     [4] expression  ::= alt
  production(:expression) do |input, current, callback|
    alt = current[:alt]
    (input[:expression] ||= [:expression]) << (alt.length > 2 ? alt : alt.last)
  end

  # Production for end of `alt` non-terminal.
  # Passes through the optimized value of the seq production as follows:
  #
  #     [:seq foo] => foo
  #     [:seq foo bar] => [:seq foo bar]
  #
  # Note that this also may just pass through from `_alt_1`
  #
  #     [5] alt         ::= seq ('|' seq)*
  production(:alt) do |input, current, callback|
    input[:alt] = if current[:alt]
      current[:alt]
    elsif seq = current[:seq]
      [:alt] << (seq.length > 2 ? seq : seq.last)
    end
  end

  # Because `alt` can refer to `seq` multiple times, we need to separate information from each sequence. To do this, we intercept the start of `_alt_1` to reset the `:seq` input and record the existing optimized `seq` value
  #
  # The generated sub-productions for `_alt_1` are:
  #
  #     (rule _alt_1 "5.1"
  #      (first _eps "|")
  #      (follow ")" "@pass" "@terminals" LHS _eof)
  #      (alt _empty _alt_3))
  start_production(:_alt_1) do |input, current, callback|
    seq = Array(input[:seq])
    (input[:alt] = [:alt]) << (seq.length > 2 ? seq : seq.last)
    input.delete(:seq)
  end

  # After `_alt_1` sub-production, add any optimized seq value and append recursive alt calls
  production(:_alt_1) do |input, current, callback|
    input[:alt] ||= [:alt]

    # Add optimized value of `seq,` if any
    if seq = current[:seq]
      input[:alt] << (seq.length == 2 ? seq.last : seq)
    end

    # Also recursive call to `_alt_1`
    input[:alt] += current[:alt][1..-1] if current[:alt]
  end

  # Production for end of `seq` non-terminal.
  # Passes through the optimized value of the `diff` production as follows:
  #
  #     [:diff foo] => foo
  #     [:diff foo bar] => [:diff foo bar]
  #
  # Note that this also may just pass through from `_seq_1`
  #
  #     [6] seq         ::= diff+
  production(:seq) do |input, current, callback|
    input[:seq] = if current[:seq]
      current[:seq]
    elsif diff = current[:diff]
      [:seq] << (diff.length > 2 ? diff : diff.last)
    end
  end

  # Because `seq` can refer to `diff `multiple times, we need to separate information from each sequence. To do this, we intercept the start of `_seq_1` to reset the `:diff` input and record the existing optimized `seq` value
  #
  # The generated sub-productions for `_seq_1` are:
  #
  #     (rule _seq_1 "6.1"
  #      (first "(" ENUM HEX O_ENUM O_RANGE RANGE STRING1 STRING2 SYMBOL _eps)
  #      (follow ")" "@pass" "@terminals" LHS _eof "|")
  #      (alt _empty _seq_2))
  start_production(:_seq_1) do |input, current, callback|
    diff = Array(input[:diff])
    (input[:seq] = [:seq]) << (diff.length > 2 ? diff : diff.last)
    input.delete(:diff)
  end

  # After `_seq_1` sub-production, add any optimized `diff` value and append recursive `seq` calls
  production(:_seq_1) do |input, current, callback|
    input[:seq] ||= [:seq]

    # Add optimized value of `diff`, if any
    if diff = current[:diff]
      input[:seq] << (diff.length > 2 ? diff : diff.last)
    end

    # Also recursive call to `_seq_1`
    input[:seq] += current[:seq][1..-1] if current[:seq]
  end

  # `Diff` production returns concatenated postfix values
  #
  #     [7] diff        ::= postfix ('-' postfix)?
  production(:diff) do |input, current, callback|
    input[:diff] = if current[:diff]
      current[:diff]
    elsif postfix = current[:postfix]
      [:diff] << postfix
    end
  end

  # Because `diff` can refer to `postfix `multiple times, we need to separate information from each sequence. To do this, we intercept the start of `_diff_1` to reset the `:postfix` input and record the existing optimized `postfix` value
  #
  # The generated sub-productions for `_diff_1` are:
  #
  #     (rule _diff_1 "7.1"
  #      (first "-" _eps)
  #      (follow "(" ")" "@pass" "@terminals" ENUM HEX LHS O_ENUM O_RANGE RANGE
  #       STRING1 STRING2 SYMBOL _eof "|" )
  #      (alt _empty _diff_2))
  start_production(:_diff_1) do |input, current, callback|
    postfix = Array(input[:postfix])
    (input[:diff] = [:diff]) << (postfix.length > 2 ? postfix : postfix.last)
    input.delete(:postfix)
  end

  # After `_diff_1` sub-production, add any optimized `diff` value and append recursive `seq` calls
  production(:_diff_1) do |input, current, callback|
    input[:diff] ||= [:diff]

    # Add optimized value of `postfix`, if any
    input[:diff] << current[:postfix] if current[:postfix]
  end

  # Production for end of `postfix` non-terminal.
  # Either returns the `primary` production value, or as modified by the `postfix`.
  #
  #     [8] postfix     ::= primary POSTFIX?
  production(:postfix) do |input, current, callback|
    # Push result onto input stack, as the `diff` production can have some number of `postfix` values that are applied recursively
    input[:postfix] =  case current[:postfix]
    when "*" then [:star, current[:primary]]
    when "+" then [:plus, current[:primary]]
    when "?" then [:opt, current[:primary]]
    else current[:primary]
    end
  end

  # Production for end of `primary` non-terminal.
  # Places `:primary` on the stack
  #
  # This may either be a terminal, or the result of an `expression`.
  #
  #     [9] primary     ::= HEX
  #                     |   SYMBOL
  #                     |   RANGE
  #                     |   ENUM
  #                     |   O_RANGE
  #                     |   O_ENUM
  #                     |   STRING1
  #                     |   STRING2
  #                     |   '(' expression ')'
  production(:primary) do |input, current, callback|
    input[:primary] = if current[:expression]
      v = current[:expression][1..-1]
      v = v.first if v.length == 1
    else
      current[:terminal]
    end
  end

  # Production for end of pass non-terminal.
  #
  #     [10] pass       ::= '@pass' expression
  production(:pass) do |input, current, callback|
    # Invoke callback
    callback.call(:pass, current[:expression].last)
  end

  # ## Parser invocation.
  # On start, yield ourselves if a block is given, otherwise, return this parser instance
  #
  # @param  [#read, #to_s]          input
  # @param  [Hash{Symbol => Object}] options
  # @option options [Hash]     :prefixes     (Hash.new)
  #   the prefix mappings to use (for acessing intermediate parser productions)
  # @option options [Boolean] :progress
  #   Show progress of parser productions
  # @return [EBNFParser]
  def initialize(input, options = {}, &block)
    @options = options.dup
    @input = input.respond_to?(:read) ? input.read : input.to_s

    parsing_terminals = false
    @ast = []
    parse(@input, START.to_sym, @options.merge(branch: BRANCH,
                                               first: FIRST,
                                               follow: FOLLOW,
                                               whitespace: EBNFParserTerminals::PASS,
                                               reset_on_true: true)
    ) do |context, *data|
      rule = case context
      when :terminal
        parsing_terminals = true
        next
      when :pass
        rule = EBNF::Rule.new(nil, nil, data.first, kind: :pass)
      when :rule
        rule = data.first
        rule.kind = :terminal if parsing_terminals
        rule
      when :trace
        level, lineno, depth, *args = data
        message = "#{args.join(': ')}"
        d_str = depth > 100 ? ' ' * 100 + '+' : ' ' * depth
        $stderr.puts "[#{lineno}](#{level})#{d_str}#{message}" if @options[:progress] || @options[:debug] == true
        next
      end
      @ast << rule
    end
    @ast
  end

  # Output formatted S-Expression of grammar
  def to_sxp
    require 'sxp' unless defined?(SXP)
    # Output rules as a formatted S-Expression
    SXP::Generator.string(@ast.sort_by{|r| r.id.to_f}.map(&:for_sxp))
  end
end
