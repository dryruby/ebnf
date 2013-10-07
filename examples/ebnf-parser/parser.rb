# EBNF Parser for EBNF.
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf/rule'
require 'ebnf/ll1/parser'
require 'meta'
require 'terminals'

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

  # Define rules for Terminals, placing results on the input stack, making them available to upstream non-Terminal rules.
  # Terminals are matched in the order of appearance
  terminal(:LHS, LHS) do |prod, token, input|
    input[:id], input[:symbol] = token.value.to_s.scan(/\[([^\]]+)\]\s*(\w+)\s*::=/).first
  end

  terminal(:SYMBOL, SYMBOL) do |prod, token, input|
    input[:terminal] = token.value.to_sym
  end

  terminal(:RANGE, RANGE) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  terminal(:ENUM, ENUM) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  terminal(:O_RANGE, O_RANGE) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  terminal(:O_ENUM, O_ENUM) do |prod, token, input|
    input[:terminal] = [:range, token.value[1..-2]]
  end

  # Strings have internal escape sequences expanded
  terminal(:STRING1, STRING1, :unescape => true) do |prod, token, input|
    input[:terminal] = token.value[1..-2]
  end

  terminal(:STRING2, STRING2, :unescape => true) do |prod, token, input|
    input[:terminal] = token.value[1..-2]
  end

  terminal(:POSTFIX, POSTFIX) do |prod, token, input|
    input[:postfix] = token.value
  end

  # String terminals defined within rules, not as explicit terminals
  terminal(nil,                  %r(@terminals|@pass|[\[\]|\-\(\)])) do |prod, token, input|
    input[:terminal] = token.value
  end

  # Define productions for non-Termainals. This can include `start_production` as well as `production` to hook into rule start and end.

  # Production for end of declaration non-terminal.
  # The `input` parameter includes information placed by previous productions at the same level, or at the start of the current production.
  # The `current` parameter, is the result of child productions placing information onto their input.
  # The `callback` parameter provides access to a callback defined in the call to `parse`, see `#each_rule` below).
  #
  # [2] declaration ::= '@terminals' | pass
  production(:declaration) do |input, current, callback|
    # If seeing "@terminals", all subsequent rules are treated as terminals
    # Otherwise, it's `pass`
  end

  # Production for end of rule non-terminal.
  # The `input` parameter includes information placed by previous productions at the same level, or at the start of the current production.
  # The `current` parameter, is the result of child productions placing information onto their input.
  # The `callback` parameter provides access to a callback defined in the call to `parse`, see `#each_rule` below).
  production(:rule) do |input, current, callback|
    # current contains a declaration
    # Invoke callback
    callback.call(:rule, EBNF::Rule.new(current[:symbol], current[:id], current[:expression].last))
  end

  production(:expression) do |input, current, callback|
    (input[:expression] ||= [:expression]) << if current[:alt].length > 2
      current[:alt]
    else
      current[:alt].last
    end
  end

  production(:alt) do |input, current, callback|
    input[:alt] = if current[:alt]
      current[:alt]
    elsif current[:seq]
      [:alt] << if Array(current[:seq]).length > 2
        current[:seq]
      else
        current[:seq].last
      end
    end
  end

  # Because alt can refer to seq multiple times, we need to separate information from each sequence. To do this, we intercept the start of _alt_1 to reset the :seq input
  start_production(:_alt_1) do |input, current, callback|
    seq = Array(input[:seq])
    (input[:alt] = [:alt]) << if seq.length > 2
      seq
    else
      seq.last
    end
    input.delete(:seq)
  end
  production(:_alt_1) do |input, current, callback|
    case Array(current[:seq]).length
    when 0, 1
      # No current value
    when 2
      (input[:alt] ||= [:alt]) << current[:seq].last
    else
      (input[:alt] ||= [:alt]) << current[:seq]
    end

    # Also recursive call to _alt_1
    case Array(current[:alt]).length
    when 0, 1
      # No current value
    when 2
      (input[:alt] ||= [:alt]) << current[:alt].last
    else
      input[:alt] ||= [:alt]
      input[:alt] += current[:alt][1..-1]
    end
  end

  production(:seq) do |input, current, callback|
    input[:seq] = if current[:seq]
      current[:seq]
    elsif current[:diff]
      [:seq] << if Array(current[:diff]).length > 2
        current[:seq]
      else
        current[:diff].last
      end
    end
  end

  # Because seq can refer to diff multiple times, we need to separate information from each sequence. To do this, we intercept the start of _seq_1 to reset the :diff input
  start_production(:_seq_1) do |input, current, callback|
    diff = Array(input[:diff])
    (input[:seq] = [:seq]) << if diff.length > 2
      diff
    else
      diff.last
    end
    input.delete(:diff)
  end
  production(:_seq_1) do |input, current, callback|
    input[:seq] ||= [:seq]
    case Array(current[:diff]).length
    when 0, 1
      # No current value
    when 2
      input[:seq] << current[:diff].last
    else
      input[:seq] << current[:diff]
    end

    # Also recursive call to _seq_1
    case Array(current[:seq]).length
    when 0, 1
      # No current value
    when 2
      input[:seq] << current[:seq].last
    else
      input[:seq] ||= [:seq]
      input[:seq] += current[:seq][1..-1]
    end
  end

  production(:diff) do |input, current, callback|
    (input[:diff] ||= [:diff]) << current[:postfix]
  end

  # Production for end of postfix non-terminal.
  # Places :postfix on the stack
  production(:postfix) do |input, current, callback|
    # Push result onto input stack, as the `diff` production can have some number of postfix values that are applied recursively
    input[:postfix] =  case current[:postfix]
    when "*" then [:star, current[:primary]]
    when "+" then [:plus, current[:primary]]
    when "?" then [:opt, current[:primary]]
    else current[:primary]
    end
  end

  # Production for end of primary non-terminal.
  # Places :primary on the stack
  #
  # This may either be a terminal, or the result of an expression
  production(:primary) do |input, current, callback|
    input[:primary] = if current[:expression]
      v = current[:expression][1..-1]
      v = v.first if v.length == 1
    else
      current[:terminal]
    end
  end

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
    if block_given?
      case block.arity
        when 0 then instance_eval(&block)
        else block.call(self)
      end
      close(@input)
    end
  end

  # Return each rule
  #
  # @yield rule
  # @yieldparam [EBNF::Rule] rule
  def each(&block)
    parsing_terminals = false
    @ast = []
    breakpoint
    parse(@input, START.to_sym, @options.merge(:branch => BRANCH,
                                               :first => FIRST,
                                               :follow => FOLLOW,
                                               :whitespace => EBNFParserTerminals::PASS,
                                               :reset_on_start => true)
    ) do |context, *data|
      rule = case context
      when :terminal
        parsing_terminals = true
        next
      when :pass
        rule = EBNF::Rule(:@pass, nil, data.first, :kind => :pass)
      when :rule
        rule = data.first
        rule.kind = :terminal if parsing_terminals
        rule
      when :trace
        level, lineno, depth, *args = data
        message = "#{args.join(': ')}"
        d_str = depth > 100 ? ' ' * 100 + '+' : ' ' * depth
        $stderr.puts "[#{lineno}](#{level})#{d_str}#{message}"
        next
      end
      @ast << rule
      yield rule
    end
  end
end
