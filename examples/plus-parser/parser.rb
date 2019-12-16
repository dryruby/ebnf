# # EBNF Parser for EBNF.
#
# Produces an Abstract Synatx Tree in S-Expression form for the input grammar file
require 'ebnf'
require 'ebnf/ll1/parser'
require 'meta'

class PlusParser
  include EBNF::LL1::Parser
  include PlusParserMeta

  # ## Terminals
  # Define rules for Terminals, placing results on the input stack, making them available to upstream non-Terminal rules.
  #
  # Terminals are matched in the order of appearance

  # Make sure we recognize string terminals, even though they're not actually used in processing
  terminal(nil,                  %r(\w+)) do |prod, token, input|
    input[:terminal] = token.value
  end

  def initialize(input, **options, &block)
    @options = options.dup
    @input = input.respond_to?(:read) ? input.read : input.to_s

    parsing_terminals = false
    @ast = []
    parse(@input, START.to_sym, branch: BRANCH,
                                first: FIRST,
                                follow: FOLLOW,
                                cleanup: CLEANUP,
                                whitespace: /\s+/,
                                **options)
    ) do |context, *data|
      rule = case context
      when :trace
        level, lineno, depth, *args = data
        message = "#{args.join(': ')}"
        d_str = depth > 100 ? ' ' * 100 + '+' : ' ' * depth
        $stderr.puts "[#{lineno}](#{level})#{d_str}#{message}" if @options[:progress] || @options[:debug] == true
        next
      end
      @ast << rule
    end

    puts prod_data.inspect
  end
end
