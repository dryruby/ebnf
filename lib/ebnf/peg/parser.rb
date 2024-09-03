module EBNF::PEG
  ##
  # A Generic PEG parser using the parsed rules modified for PEG parseing.
  module Parser
    ##
    # @return [Regexp, Rule] how to remove inter-rule whitespace
    attr_reader :whitespace

    ##
    # @return [Scanner] used for scanning input.
    attr_reader :scanner

    ##
    # A Hash structure used for memoizing rule results for a given input location.
    #
    #  @example Partial structure for memoizing results for a particular rule
    #
    #      {
    #        rule: {
    #          86: {
    #                pos: 
    #                result: [<EBNF::Rule:80 {
    #                  sym: :ebnf,
    #                    id: "1",
    #                    kind: :rule,
    #                    expr: [:star, [:alt, :declaration, :rule]]}>],
    #               }
    #          131: [<EBNF::Rule:80 {sym: :ebnf,
    #              id: "1",
    #              kind: :rule,
    #              expr: [:star, [:alt, :declaration, :rule]]}>,
    #            <EBNF::Rule:100 {
    #              sym: :declaration,
    #              id: "2",
    #              kind: :rule,
    #              expr: [:alt, "@terminals", :pass]}>]
    #        },
    #        POSTFIX: {
    #          80: "*",
    #          368: "*",
    #          399: "+"
    #        }
    #      }
    # @return [Hash{Integer => Hash{Symbol => Object}}]
    attr_reader :packrat

    def self.included(base)
      base.extend(ClassMethods)
    end

    # DSL for creating terminals and productions
    module ClassMethods
      def start_handlers; (@start_handlers ||= {}); end
      def start_options; (@start_hoptions ||= {}); end
      def production_handlers; (@production_handlers ||= {}); end
      def terminal_handlers; (@terminal_handlers ||= {}); end
      def terminal_regexps; (@terminal_regexps ||= {}); end
      def terminal_options; (@terminal_options ||= {}); end

      ##
      # Defines the pattern for a terminal node and a block to be invoked
      # when ther terminal is encountered. If the block is missing, the
      # value of the terminal will be placed on the input hash to be returned
      # to a previous production. Block is called in an evaluation block from
      # the enclosing parser.
      #
      # If no block is provided, then the value which would have been passed to the block is used as the result directly.
      #
      # @param [Symbol] term
      #   The terminal name.
      # @param [Regexp] regexp (nil)
      #   Pattern used to scan for this terminal,
      #   defaults to the expression defined in the associated rule.
      #   If unset, the terminal rule is used for matching.
      # @param [Hash] options
      # @option options [Boolean] :unescape
      #   Cause strings and codepoints to be unescaped.
      # @yield [value, prod]
      # @yieldparam [String] value
      #   The scanned terminal value.
      # @yieldparam [Symbol] prod
      #   A symbol indicating the production which referenced this terminal
      # @yieldparam [Proc] block
      #   Block passed to initialization for yielding to calling parser.
      #   Should conform to the yield specs for #initialize
      def terminal(term, regexp = nil, **options, &block)
        terminal_regexps[term] = regexp if regexp
        terminal_handlers[term] = block if block_given?
        terminal_options[term] = options.freeze
      end

      ##
      # Defines a production called at the beggining of a particular production
      # with data from previous production along with data defined for the
      # current production. Block is called in an evaluation block from
      # the enclosing parser.
      #
      # @param [Symbol] term
      #   The rule name
      # @param [Hash{Symbol => Object}] options
      #   Options which are returned from {Parser#onStart}.
      # @option options [Boolean] :as_hash (false)
      #   If the production is a `seq`, causes the value to be represented as a single hash, rather than an array of individual hashes for each sub-production. Note that this is not always advisable due to the possibility of repeated productions within the sequence.
      # @option options[:upper, :lower] :insensitive_strings
      #   Perform case-insensitive match of strings not defined as terminals, and map to either upper or lower case.
      # @yield [data, block]
      # @yieldparam [Hash] data
      #   A Hash defined for the current production, during :start
      #   may be initialized with data to pass to further productions,
      #   during :finish, it contains data placed by earlier productions
      # @yieldparam [Proc] block
      #   Block passed to initialization for yielding to calling parser.
      #   Should conform to the yield specs for #initialize
      # Yield to generate a triple
      def start_production(term, **options, &block)
        start_handlers[term] = block
        start_options[term] = options.freeze
      end

      ##
      # Defines a production called when production of associated
      # non-terminals has completed
      # with data from previous production along with data defined for the
      # current production. Block is called in an evaluation block from
      # the enclosing parser.
      #
      # @param [Symbol] term
      #   Term which is a key in the branch table
      # @param [Boolean] clear_packrat (false)
      #   Clears the packrat state on completion to reduce memory requirements of parser. Use only on a top-level rule when it is determined that no further backtracking is necessary.
      # @yield [result, data, block]
      # @yieldparam [Object] result
      #   The result from sucessfully parsing the production.
      # @yieldparam [Hash] data
      #   A Hash defined for the current production, during :start
      #   may be initialized with data to pass to further productions,
      #   during :finish, it contains data placed by earlier productions
      # @yieldparam [Proc] block
      #   Block passed to initialization for yielding to calling parser.
      #   Should conform to the yield specs for #initialize
      # @yieldreturn [Object] the result of this production.
      # Yield to generate a triple
      def production(term, clear_packrat: false, &block)
        production_handlers[term] = [block, clear_packrat]
      end

      # Evaluate a handler, delegating to the specified object.
      # This is necessary so that handlers can operate within the
      # binding context of the parser in which they're invoked.
      # @param [Object] object
      # @return [Object]
      def eval_with_binding(object)
        @delegate = object
        object.instance_eval {yield}
      end

      private

      def method_missing(method, *args, &block)
        if @delegate ||= nil
          # special handling when last arg is **options
          params = @delegate.method(method).parameters
          if params.any? {|t, _| t == :keyrest} && args.last.is_a?(Hash)
            opts = args.pop
            @delegate.send(method, *args, **opts, &block)
          else
            @delegate.send(method, *args, &block)
          end
        else
          super
        end
      end
    end

    ##
    # Initializes a new parser instance.
    #
    # @param  [String, #to_s] input
    # @param [Symbol, #to_s] start
    #   The starting production for the parser. It may be a URI from the grammar, or a symbol representing the local_name portion of the grammar URI.
    # @param [Array<EBNF::PEG::Rule>] rules
    #   The parsed rules, which control parsing sequence.
    #   Identify the symbol of the starting rule with `start`.
    # @param  [Hash{Symbol => Object}] options
    # @option options[Integer] :high_water passed to lexer
    # @option options [Logger] :logger for errors/progress/debug.
    # @option options[Integer] :low_water passed to lexer
    # @option options[Boolean] :seq_hash (false)
    #   If `true`, sets the default for the value sent to a production handler that is for a `seq` to a hash composed of the flattened consitutent hashes that are otherwise provided.
    # @option options [Symbol, Regexp] :whitespace 
    #   Symbol of whitespace rule (defaults to `@pass`), or a regular expression
    #   for eating whitespace between non-terminal rules (strongly encouraged).
    # @yield [context, *data]
    #   Yields to return data to parser
    # @yieldparam [:statement, :trace] context
    #   Context for block
    # @yieldparam [Symbol] *data
    #   Data specific to the call
    # @return [Object] AST resulting from parse
    # @raise [Exception] Raises exceptions for parsing errors
    #   or errors raised during processing callbacks. Internal
    #   errors are raised using {Error}.
    # @todo FIXME implement seq_hash
    def parse(input = nil, start = nil, rules = nil, **options, &block)
      start ||= options[:start]
      rules ||= options[:rules] || []
      @rules = rules.inject({}) {|memo, rule| memo.merge(rule.sym => rule)}
      @packrat = {}

      # Add parser reference to each rule
      @rules.each_value {|rule| rule.parser = self}

      # Take whitespace from options, a named rule, a `pass` rule, a rule named :WS, or a default
      @whitespace = case options[:whitespace]
      when Regexp then options[:whitespace]
      when Symbol then @rules[options[:whitespace]]
      else options[:whitespace]
      end ||
        @rules.values.detect(&:pass?) ||
        /(?:\s|(?:#[^x][^\n\r]*))+/m.freeze

      @options = options.dup
      @productions = []
      @parse_callback = block
      @error_log = []
      @prod_data = []

      @scanner = EBNF::LL1::Scanner.new(input)
      start = start.split('#').last.to_sym unless start.is_a?(Symbol)
      start_rule = @rules[start]
      raise Error, "Starting production #{start.inspect} not defined" unless start_rule

      result = start_rule.parse(scanner)
      if result == :unmatched
        # Start rule wasn't matched, which is about the only error condition
        error("--top--", @furthest_failure.to_s,
          pos: @furthest_failure.pos,
          lineno: @furthest_failure.lineno,
          rest: scanner.string[@furthest_failure.pos, 20])
      end

      # Eat any remaining whitespace
      start_rule.eat_whitespace(scanner)
      if !scanner.eos?
        error("--top--", @furthest_failure.to_s,
          pos: @furthest_failure.pos,
          lineno: @furthest_failure.lineno,
          rest: scanner.string[@furthest_failure.pos, 20])
      end

      # When all is said and done, raise the error log
      unless @error_log.empty?
        raise Error, @error_log.join("\n")
      end

      result
    end

    # Depth of parsing, for log output.
    def depth; (@productions || []).length; end

    # Current ProdData element
    def prod_data; @prod_data.last || {}; end

    # Clear out packrat memoizer. This is appropriate when completing a top-level rule when there is no possibility of backtracking.
    def clear_packrat; @packrat.clear; end

    ##
    # Error information, used as level `3` logger messages.
    # Messages may be logged and are saved for reporting at end of parsing.
    #
    # @param [String] node Relevant location associated with message
    # @param [String] message Error string
    # @param [Hash{Symbol => Object}] options
    # @option options [URI, #to_s] :production
    # @option options [Boolean] :raise abort furhter processing
    # @option options [Array] :backtrace state where error occured
    # @see #debug
    def error(node, message, **options)
      lineno = options[:lineno] || (scanner.lineno if scanner)
      m = "ERROR "
      m += "[line: #{lineno}] " if lineno
      m += message
      m += " (found #{options[:rest].inspect})" if options[:rest]
      m += ", production = #{options[:production].inspect}" if options[:production]
      @error_log << m unless @recovering
      @recovering = true
      debug(node, m, level: 3, **options)
      if options[:raise] || @options[:validate]
        raise Error.new(m,
                lineno: lineno,
                rest: options[:rest],
                production: options[:production],
                backtrace: options[:backtrace])
      end
    end

    ##
    # Warning information, used as level `2` logger messages.
    # Messages may be logged and are saved for reporting at end of parsing.
    #
    # @param [String] node Relevant location associated with message
    # @param [String] message Error string
    # @param [Hash] options
    # @option options [URI, #to_s] :production
    # @option options [Token] :token
    # @see #debug
    def warn(node, message, **options)
      lineno = options[:lineno] || (scanner.lineno if scanner)
      m = "WARNING "
      m += "[line: #{lineno}] " if lineno
      m += message
      m += " (found #{options[:rest].inspect})" if options[:rest]
      m += ", production = #{options[:production].inspect}" if options[:production]
      debug(node, m, level: 2, **options)
    end

    ##
    # Progress logged when parsing. Passed as level `1` logger messages.
    #
    # The call is ignored, unless `@options[:logger]` is set.
    #
    # @overload progress(node, message, **options, &block)
    #   @param [String] node Relevant location associated with message
    #   @param [String] message ("")
    #   @param [Hash] options
    #   @option options [Integer] :depth
    #       Recursion depth for indenting output
    # @see #debug
    def progress(node, *args, &block)
      return unless @options[:logger]
      args << {} unless args.last.is_a?(Hash)
      args.last[:level] ||= 1
      debug(node, *args, &block)
    end

    ##
    # Debug logging.
    #
    # The call is ignored, unless `@options[:logger]` is set.
    #
    # @overload debug(node, message, **options)
    #   @param [Array<String>] args Relevant location associated with message
    #   @param [Hash] options
    #   @option options [Integer] :depth
    #     Recursion depth for indenting output
    #   @yieldreturn [String] additional string appended to `message`.
    def debug(*args, &block)
      return unless @options[:logger]
      options = args.last.is_a?(Hash) ? args.pop : {}
      lineno = options[:lineno] || (scanner.lineno if scanner)
      level = options.fetch(:level, 0)
      depth = options[:depth] || self.depth

      if self.respond_to?(:log_debug)
        level = [:debug, :info, :warn, :error, :fatal][level]
        log_debug(*args, **options.merge(level: level, lineno: lineno, depth: depth), &block)
      elsif @options[:logger].respond_to?(:add)
        args << yield if block_given?
        @options[:logger].add(level, "[#{lineno}]" + (" " * depth) + args.join(" "))
      elsif @options[:logger].respond_to?(:<<)
        args << yield if block_given?
        @options[:logger] << "[#{lineno}]" + (" " * depth) + args.join(" ")
      end
    end

    # Start for production
    # Adds data avoiable during the processing of the production
    #
    # @return [Hash] composed of production options. Currently only `as_hash` is supported.
    # @see ClassMethods#start_production
    def onStart(prod)
      handler = self.class.start_handlers[prod]
      @productions << prod
      progress("#{prod}(:start)", "",
        lineno: (scanner.lineno if scanner),
        pos: (scanner.pos if scanner)
      ) do
          "#{prod}, pos: #{scanner ? scanner.pos : '?'}, rest: #{scanner ? scanner.rest[0..20].inspect : '?'}"
      end
      if handler
        # Create a new production data element, potentially allowing handler
        # to customize before pushing on the @prod_data stack
        data = {_production: prod}
        begin
          self.class.eval_with_binding(self) {
            handler.call(data, @parse_callback)
          }
        rescue ArgumentError, Error => e
          error("start", "#{e.class}: #{e.message}", production: prod, backtrace: e.backtrace)
          @recovering = false
        end
        @prod_data << data
      elsif self.class.production_handlers[prod]
        # Make sure we push as many was we pop, even if there is no
        # explicit start handler
        @prod_data << {_production: prod}
      end
      return self.class.start_options.fetch(prod, {}) # any options on this production
    end

    # Finish of production
    #
    # @param [Object] result parse result
    # @return [Object] parse result, or the value returned from the handler
    def onFinish(result)
      #puts "prod_data(f): " + @prod_data.inspect
      prod = @productions.last
      handler, clear_packrat = self.class.production_handlers[prod]
      data = @prod_data.pop if handler || self.class.start_handlers[prod]
      error("finish",
        "prod_data production mismatch: expected #{prod.inspect}, got #{data[:_production].inspect}",
        production: prod, prod_data: @prod_data) if data && prod != data[:_production]
      if handler && !@recovering && result != :unmatched
        # Pop production data element from stack, potentially allowing handler to use it
        result = begin
          self.class.eval_with_binding(self) {
            handler.call(result, data, @parse_callback)
          }
        rescue ArgumentError, Error => e
          error("finish", "#{e.class}: #{e.message}", production: prod, backtrace: e.backtrace)
          @recovering = false
        end
      end
      progress("#{prod}(:finish)", "",
             lineno: (scanner.lineno if scanner),
             level: result == :unmatched ? 0 : 1) do
        "#{result.inspect}@(#{scanner ? scanner.pos : '?'}), rest: #{scanner ? scanner.rest[0..20].inspect : '?'}"
      end
      self.clear_packrat if clear_packrat
      @productions.pop
      result
    end

    # A terminal with a defined handler
    #
    # @param [Symbol] prod from the symbol of the associated rule
    # @param [String] value the scanned string
    # @return [String, Object] either the result from the handler, or the token
    def onTerminal(prod, value)
      parentProd = @productions.last
      handler = self.class.terminal_handlers[prod]
      if handler && value != :unmatched
        value = begin
          self.class.eval_with_binding(self) {
            handler.call(value, parentProd, @parse_callback)
          }
        rescue ArgumentError, Error => e
          error("terminal", "#{e.class}: #{e.message}", value: value, production: prod, backtrace: e.backtrace)
          @recovering = false
        end
      end
      progress("#{prod}(:terminal)", "",
               depth: (depth + 1),
               lineno: (scanner.lineno if scanner),
               level: value == :unmatched ? 0 : 1) do
        "#{value.inspect}@(#{scanner ? scanner.pos : '?'})"
      end
      value
    end

    ##
    # Find a rule for a symbol
    #
    # @param [Symbol] sym
    # @return [Rule]
    def find_rule(sym)
      @rules[sym]
    end

    ##
    # Find a regular expression defined for a terminal
    #
    # @param [Symbol] sym
    # @return [Regexp]
    def terminal_regexp(sym)
      self.class.terminal_regexps[sym]
    end

    ##
    # Find a regular expression defined for a terminal
    #
    # @param [Symbol] sym
    # @return [Regexp]
    def terminal_options(sym)
      self.class.terminal_options[sym]
    end

    ##
    # Record furthest failure.
    #
    # @param [Integer] pos
    #   The position in the input stream where the failure occured.
    # @param [Integer] lineno
    #   Line where the failure occured.
    # @param [Symbol, String] token
    #   The terminal token or string which attempted to match.
    # @see https://arxiv.org/pdf/1405.6646.pdf
    def update_furthest_failure(pos, lineno, token)
      # Skip generated productions
      return if token.is_a?(Symbol) && token.to_s.start_with?('_')
      if @furthest_failure.nil? || pos > @furthest_failure.pos
        @furthest_failure = Unmatched.new(pos, lineno, [token])
      elsif pos == @furthest_failure.pos && !@furthest_failure[:expecting].include?(token)
        @furthest_failure[:expecting] << token
      end
    end

  public

    ##
    # @!parse
    #   # Record details about an inmatched rule, including the following:
    #   # 
    #   # * Input location and line number at time of failure.
    #   # * The rule at which this was found (non-terminal, and nat starting with '_').
    #   class Unmatched
    #     # @return [Integer] The position within the scanner which did not match.
    #     attr_reader :pos
    #     # @return [Integer] The line number which did not match.
    #     attr_reader :lineno
    #     # @return [Array<Symbol,String>]
    #     #   Strings or production rules that attempted to match at this position.
    #     attr_reader :expecting
    #   end
    class Unmatched < Struct.new(:pos, :lineno, :expecting)
      def to_s
        "syntax error, expecting #{expecting.map(&:inspect).join(', ')}"
      end
    end

    ##
    # Raised for errors during parsing.
    #
    # @example Raising a parser error
    #   raise Error.new(
    #     "invalid token '%' on line 10",
    #     rest: '%', lineno: 9, production: :turtleDoc)
    #
    # @see https://ruby-doc.org/core/classes/StandardError.html
    class Error < StandardError
      ##
      # The current production.
      #
      # @return [Symbol]
      attr_reader :production

      ##
      # The read head when scanning failed
      #
      # @return [String]
      attr_reader :rest

      ##
      # The line number where the error occurred.
      #
      # @return [Integer]
      attr_reader :lineno

      ##
      # Initializes a new lexer error instance.
      #
      # @param  [String, #to_s]          message
      # @param  [Hash{Symbol => Object}] options
      # @option options [Symbol]         :production  (nil)
      # @option options [String]         :rest  (nil)
      # @option options [Integer]        :lineno (nil)
      def initialize(message, **options)
        @production = options[:production]
        @rest       = options[:rest]
        @lineno     = options[:lineno]
        super(message.to_s)
      end
    end # class Error
  end # class Parser
end # module EBNF::LL1
