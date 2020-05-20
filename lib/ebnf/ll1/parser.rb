require 'ebnf/ll1/lexer'

module EBNF::LL1
  ##
  # A Generic LL1 parser using a lexer and branch tables defined using the SWAP tool chain (modified).
  module Parser
    ##
    # @private
    # level above which debug messages are supressed
    DEBUG_LEVEL = 10

    ##
    # @return [Integer] line number of current token
    attr_reader :lineno

    def self.included(base)
      base.extend(ClassMethods)
    end

    # DSL for creating terminals and productions
    module ClassMethods
      def start_handlers; @start_handlers || {}; end
      def production_handlers; @production_handlers || {}; end
      def terminal_handlers; @terminal_handlers || {}; end
      def patterns; @patterns || []; end

      ##
      # Defines the pattern for a terminal node and a block to be invoked
      # when ther terminal is encountered. If the block is missing, the
      # value of the terminal will be placed on the input hash to be returned
      # to a previous production. Block is called in an evaluation block from
      # the enclosing parser.
      #
      # @param [Symbol, String] term
      #   Defines a terminal production, which appears as within a sequence in the branch table
      # @param [Regexp] regexp
      #   Pattern used to scan for this terminal
      # @param [Hash] options
      # @option options [Hash{String => String}] :map ({})
      #   A mapping from terminals, in lower-case form, to
      #   their canonical value
      # @option options [Boolean] :unescape
      #   Cause strings and codepoints to be unescaped.
      # @yield [term, token, input, block]
      # @yieldparam [Symbol] term
      #   A symbol indicating the production which referenced this terminal
      # @yieldparam [String] token
      #   The scanned token
      # @yieldparam [Hash] input
      #   A Hash containing input from the parent production
      # @yieldparam [Proc] block
      #   Block passed to initialization for yielding to calling parser.
      #   Should conform to the yield specs for #initialize
      def terminal(term, regexp, **options, &block)
        @patterns ||= []
        # Passed in order to define evaulation sequence
        @patterns << EBNF::LL1::Lexer::Terminal.new(term, regexp, **options)
        @terminal_handlers ||= {}
        @terminal_handlers[term] = block if block_given?
      end

      ##
      # Defines a production called at the beggining of a particular production
      # with data from previous production along with data defined for the
      # current production. Block is called in an evaluation block from
      # the enclosing parser.
      #
      # @param [Symbol] term
      #   Term which is a key in the branch table
      # @yield [input, current, block]
      # @yieldparam [Hash] input
      #   A Hash containing input from the parent production
      # @yieldparam [Hash] current
      #   A Hash defined for the current production, during :start
      #   may be initialized with data to pass to further productions,
      #   during :finish, it contains data placed by earlier productions
      # @yieldparam [Proc] block
      #   Block passed to initialization for yielding to calling parser.
      #   Should conform to the yield specs for #initialize
      # Yield to generate a triple
      def start_production(term, &block)
        @start_handlers ||= {}
        @start_handlers[term] = block
      end

      ##
      # Defines a production called when production of associated
      # terminals and non-terminals has completed
      # with data from previous production along with data defined for the
      # current production. Block is called in an evaluation block from
      # the enclosing parser.
      #
      # @param [Symbol] term
      #   Term which is a key in the branch table
      # @yield [input, current, block]
      # @yieldparam [Hash] input
      #   A Hash containing input from the parent production
      # @yieldparam [Hash] current
      #   A Hash defined for the current production, during :start
      #   may be initialized with data to pass to further productions,
      #   during :finish, it contains data placed by earlier productions
      # @yieldparam [Proc] block
      #   Block passed to initialization for yielding to calling parser.
      #   Should conform to the yield specs for #initialize
      # Yield to generate a triple
      def production(term, &block)
        @production_handlers ||= {}
        @production_handlers[term] = block
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
    # Attempts to recover from errors.
    #
    # @example
    #   require 'rdf/ll1/parser'
    #
    #   class MyParser
    #     include EBNF::LL1::Parser
    #
    #     branch      MyParser::BRANCH
    #
    #     ##
    #     # Defines a production called during before parsing a non-terminal
    #     # with data from previous production along with data defined for the
    #     # current production
    #     #
    #     start_production :object do |input, current, callback|
    #       # Note production as triples for blankNodePropertyList
    #       # to set :subject instead of :resource
    #       current[:triples] = true
    #     end
    #
    #     ##
    #     # Defines a production called during after parsing a non-terminal
    #     # with data from previous production along with data defined for the
    #     # current production
    #     #
    #     # callback to processor block
    #     production :object do |input, current, callback|
    #       object = current[:resource]
    #       callback.call :statement, RDF::Statement.new(input[:subject], input[:predicate], object)
    #     end
    #
    #     ##
    #     # Defines the pattern for a terminal node
    #     terminal :BLANK_NODE_LABEL, %r(_:(#{PN_LOCAL})) do |production, token, input|
    #       input[:BLANK_NODE_LABEL] = RDF::Node.new(token)
    #     end
    #
    #     ##
    #     # Iterates the given block for each RDF statement in the input.
    #     #
    #     # @yield  [statement]
    #     # @yieldparam [RDF::Statement] statement
    #     # @return [void]
    #     def each_statement(&block)
    #       @callback = block
    #
    #       parse(START.to_sym) do |context, *data|
    #         case context
    #         when :statement
    #           yield *data
    #         end
    #       end
    #     end
    #
    #   end
    #
    # @param  [String, #to_s]          input
    # @param [Symbol, #to_s] start
    #   The starting production for the parser. It may be a URI from the grammar, or a symbol representing the local_name portion of the grammar URI.
    # @param  [Hash{Symbol => Object}] options
    # @option options [Hash{Symbol,String => Hash{Symbol,String => Array<Symbol,String>}}] :branch LL1 branch table.
    # @option options [HHash{Symbol,String => Array<Symbol,String>}] :first ({})
    #   Lists valid terminals that can precede each production (for error recovery).
    # @option options [Hash{Symbol,String => Array<Symbol,String>}] :follow ({})
    #   Lists valid terminals that can follow each production (for error recovery).
    # @option options [Boolean]  :validate     (false)
    #   whether to validate the parsed statements and values. If not validating, the parser will attempt to recover from errors.
    # @option options [Boolean] :progress
    #   Show progress of parser productions
    # @option options [Boolean] :debug
    #   Detailed debug output
    # @option options [Boolean] :reset_on_start
    #   Reset the parser state if the start token set with `prod` is found in a production. This reduces the production stack depth growth, which is appropriate for some grammars.
    # @option options[Integer] :high_water passed to lexer
    # @option options[Integer] :low_water passed to lexer
    # @yield [context, *data]
    #   Yields for to return data to parser
    # @yieldparam [:statement, :trace] context
    #   Context for block
    # @yieldparam [Symbol] *data
    #   Data specific to the call
    # @return [EBNF::LL1::Parser]
    # @raise [Exception] Raises exceptions for parsing errors
    #   or errors raised during processing callbacks. Internal
    #   errors are raised using {Error}.
    # @see https://cs.adelaide.edu.au/~charles/lt/Lectures/07-ErrorRecovery.pdf
    def parse(input = nil, start = nil, **options, &block)
      @options = options.dup
      @options[:debug] ||= case
      when @options[:progress] then 2
      when @options[:validate] then 1
      end
      @branch  = options[:branch]
      @first   = options[:first] ||= {}
      @follow  = options[:follow] ||= {}
      @cleanup = options[:cleanup] ||= {}
      @lexer   = input.is_a?(Lexer) ? input : Lexer.new(input, self.class.patterns, **@options)
      @productions = []
      @parse_callback = block
      @recovering = false
      @error_log = []
      terminals = self.class.patterns.map(&:type)  # Get defined terminals to help with branching

      # Unrecoverable errors
      raise Error, "Branch table not defined" unless @branch && @branch.length > 0
      raise Error, "Starting production not defined" unless start

      @prod_data = [{}]
      start = start.split('#').last.to_sym unless start.is_a?(Symbol)
      todo_stack = [{prod: start, terms: nil}]

      while !todo_stack.empty?
        begin
          @recovering = false
          pushed = false
          if todo_stack.last[:terms].nil?
            todo_stack.last[:terms] = []
            cur_prod = todo_stack.last[:prod]

            # If cur_prod is the starting production, we can reset the stack
            # to the beginning to avoid excessive growth in the production
            # stack
            if options[:reset_on_start] && cur_prod == start
              todo_stack = [{prod: start, terms: []}]
              @productions = []
              @prod_data = [{}]
            end

            # Fetch the current token
            token = get_token(:recover)

            # At this point, token is either nil, in the first set of the production,
            # or in the follow set of this production or any previous production
            debug("parse(production)") do
              "token #{token ? token.representation.inspect : 'nil'}, " +
              "prod #{cur_prod.inspect}, " +
              "depth #{depth}"
            end

            # Got an opened production
            onStart(cur_prod)

            if token.nil?
              if  !(first_include?(cur_prod, :_eps) && follow_include?(cur_prod, :_eof))
                # End of file, and production does not contain eps, or it does, but follow does not contain eof
                error("parse(production)", "Unexpected end of input", lineno: lineno, production: cur_prod, raise: true)
              else
                debug("parse(production)") {"End of input prod #{cur_prod.inspect}"}
              end
            elsif prod_branch = @branch[cur_prod]
              sequence = prod_branch.fetch(token.representation) do
                error("parse(production)", "Expected one of #{@first[cur_prod].inspect}", token: token, production: cur_prod, raise: true)
              end
              debug("parse(production)") do
                "token #{token.representation.inspect} " +
                "prod #{cur_prod.inspect}, " +
                "prod_branch #{prod_branch.keys.inspect}, " +
                "sequence #{sequence.inspect}"
              end
              todo_stack.last[:terms] += sequence
            else
              error("parse(production)", "Unexpected", token: token, production: cur_prod, raise: true)
            end
          end

          debug("parse(terms)") {"todo #{todo_stack.last.inspect}, depth #{depth}"}
          while !todo_stack.last[:terms].to_a.empty?
            # Get the next term in this sequence
            term = todo_stack.last[:terms].shift
            debug("parse(token)") {"accept #{term.inspect}"}

            if token = accept(term)
              debug("parse(token)") {"token #{token.inspect}, term #{term.inspect}"}
              onTerminal(term, token)
            elsif terminals.include?(term) 
              # If term is a terminal, then it is an error if token does not
              # match it
              error("parse(token)", "Expected #{term.inspect}", token: get_token, production: cur_prod, raise: true)
            else
              token = get_token

              # If token is not in firsts of term, but eps is, skip to next
              # term
              if first_include?(term, :_eps) && !first_include?(term, token)
                debug("parse(token)") {"skip optional term #{term.inspect} on #{token.inspect}"}
                break
              else
                # Push term onto stack
                todo_stack << {prod: term, terms: nil}
                debug("parse(push)") {"term #{term.inspect}, depth #{depth}"}
                pushed = true
                break
              end
            end
          end
        rescue Lexer::Error, Error => e
          # Lexer encountered an illegal token or the parser encountered
          # a terminal which is inappropriate for the current production.
          # Perform error recovery to find a reasonable terminal based
          # on the follow sets of the relevant productions. This includes
          # remaining terms from the current production and the stacked
          # productions
          @lineno = e.lineno
          if e.is_a?(Lexer::Error)
            # Skip to the next valid terminal
            @lexer.recover
            error("parse(#{e.class})", "With input '#{e.input}': #{e.message}",
                  production: @productions.last, token: e.token)
          else
            # Otherwise, the terminal is fine, just not for this production.
            @lexer.shift
            error("parse(#{e.class})", "#{e.message}",
                  production: @productions.last, token: e.token)
          end

          # Get the list of follows for this sequence, this production and the stacked productions.
          debug("recovery", "stack follows:", level: 4)
          todo_stack.reverse.each do |todo|
            debug("recovery", level: 4) {"  #{todo[:prod]}: #{@follow[todo[:prod]].inspect}"}
          end

          # Find all follows to the top of the stack
          follows = todo_stack.inject([]) do |follow, todo|
            prod = todo[:prod]
            follow += @follow[prod] || []
          end.uniq
          debug("recovery") {"follows: #{follows.inspect}"}

          # Skip tokens until one is found in follows
          while (token = get_token(:recover)) && follows.none? {|t| token === t}
            skipped = @lexer.shift
            progress("recovery") {"skip #{skipped.inspect}"}
          end
          debug("recovery") {"found #{token.inspect} in follows"}

          # Pop stack elements until token is in follows
          while !todo_stack.empty? &&
                !follow_include?(todo_stack.last[:prod], token || :_eof)
            debug("recovery(pop)") {"todo #{todo_stack.last.inspect}, depth #{depth}"}
            todo_stack.pop
            onFinish
          end

          # Token is now in the first of the top production
          unless todo_stack.empty?
            todo_stack.pop
            onFinish
          end

          if todo_stack.empty?
            # Recovered to end of last production
            warn("recover", "recovered to end of productions")
          else
            warn("recover", "recovered to #{todo_stack.last[:prod].inspect} with #{token.inspect}")
          end

          @recovering = false
        ensure
          # After completing the last production in a sequence, pop down until we find a production
          #
          # If in recovery mode, continue popping until we find a term with a follow list
          while !pushed &&
                !todo_stack.empty? &&
                todo_stack.last.fetch(:terms, []).empty?
            debug("parse(pop)") {"todo #{todo_stack.last.inspect}, depth #{depth}"}
            todo_stack.pop
            onFinish
          end
        end
      end

      error("parse(eof)", "Finished processing before end of file", token: @lexer.first) if @lexer.first

      # Continue popping contexts off of the stack
      while !todo_stack.empty?
        debug("parse(eof)") {"stack #{todo_stack.last.inspect}, depth #{depth}"}
        # There can't be anything left to do, or if there is, it must be optional
        last_terms = todo_stack.last[:terms]
        if last_terms.length > 0 && last_terms.none? {|t|first_include?(t, :_eps)}
          error("parse(eof)",
            "End of input before end of production: stack #{todo_stack.last.inspect}, depth #{depth}"
          )
        end
        todo_stack.pop
        onFinish
      end

      # When all is said and done, raise the error log
      unless @error_log.empty?
        raise Error, @error_log.join("\n")
      end
    end

    def depth; (@productions || []).length; end

    # Current ProdData element
    def prod_data; @prod_data.last; end

    # Add a single value to prod_data, allows for values to be an array
    def add_prod_datum(sym, values)
      case values
      when Array
        prod_data[sym] ||= []
        debug("add_prod_datum(#{sym})") {"#{prod_data[sym].inspect} += #{values.inspect}"}
        prod_data[sym] += values
      when nil
        return
      else
        prod_data[sym] ||= []
        debug("add_prod_datum(#{sym})") {"#{prod_data[sym].inspect} << #{values.inspect}"}
        prod_data[sym] << values
      end
    end

    # Add values to production data, values aranged as an array
    def add_prod_data(sym, *values)
      return if values.compact.empty?

      prod_data[sym] ||= []
      prod_data[sym] += values
      debug("add_prod_data(#{sym})") {"#{prod_data[sym].inspect} += #{values.inspect}"}
    end

  protected

    ##
    # Error information, used as level `0` debug messages.
    #
    # @param [String] node Relevant location associated with message
    # @param [String] message Error string
    # @param [Hash{Symbol => Object}] options
    # @option options [URI, #to_s] :production
    # @option options [Token] :token
    # @see #debug
    def error(node, message, **options)
      lineno = @lineno || (options[:token].lineno if options[:token].respond_to?(:lineno))
      m = "ERROR "
      m += "[line: #{lineno}] " if lineno
      m += message
      m += " (found #{options[:token].inspect})" if options[:token]
      m += ", production = #{options[:production].inspect}" if options[:production]
      @error_log << m unless @recovering
      @recovering = true
      debug(node, m, level: 0, **options)
      if options[:raise] || @options[:validate]
        raise Error.new(m, lineno: lineno, token: options[:token], production: options[:production])
      end
    end

    ##
    # Warning information, used as level `1` debug messages.
    #
    # @param [String] node Relevant location associated with message
    # @param [String] message Error string
    # @param [Hash] options
    # @option options [URI, #to_s] :production
    # @option options [Token] :token
    # @see #debug
    def warn(node, message, **options)
      m = "WARNING "
      m += "[line: #{@lineno}] " if @lineno
      m += message
      m += " (found #{options[:token].inspect})" if options[:token]
      m += ", production = #{options[:production].inspect}" if options[:production]
      @error_log << m unless @recovering
      debug(node, m, level: 1, **options)
    end

    ##
    # Progress output when parsing. Passed as level `2` debug messages.
    #
    # @overload progress(node, message, **options)
    #   @param [String] node Relevant location associated with message
    #   @param [String] message ("")
    #   @param [Hash] options
    #   @option options [Integer] :depth
    #       Recursion depth for indenting output
    # @see #debug
    def progress(node, *args, &block)
      return unless @options[:progress] || @options[:debug]
      args << {} unless args.last.is_a?(Hash)
      args.last[:level] ||= 2
      debug(node, *args, &block)
    end

    ##
    # Progress output when debugging.
    #
    # The call is ignored, unless `@options[:debug]` is set, in which
    # case it yields tracing information as indicated. Additionally,
    # if `@options[:debug]` is an Integer, the call is aborted if the
    # `:level` option is less than than `:level`.
    #
    # @overload debug(node, message, **options)
    #   @param [Array<String>] args Relevant location associated with message
    #   @param [Hash] options
    #   @option options [Integer] :depth
    #     Recursion depth for indenting output
    #   @option options [Integer] :level
    #     Level assigned to message, by convention, level `0` is for
    #     errors, level `1` is for warnings, level `2` is for parser
    #     progress information, and anything higher is for various levels
    #     of debug information.
    #
    # @yield trace, level, lineno, depth, args
    # @yieldparam [:trace] trace
    # @yieldparam [Integer] level
    # @yieldparam [Integer] lineno
    # @yieldparam [Integer] depth Recursive depth of productions
    # @yieldparam [Array<String>] args
    # @yieldreturn [String] added to message
    def debug(*args)
      return unless @options[:debug] && @parse_callback
      options = args.last.is_a?(Hash) ? args.pop : {}
      debug_level = options.fetch(:level, 3)
      return if @options[:debug].is_a?(Integer) && debug_level > @options[:debug]

      depth = options[:depth] || self.depth
      args << yield if block_given?
      @parse_callback.call(:trace, debug_level, @lineno, depth, *args)
    end

  private
    # Start for production
    def onStart(prod)
      handler = self.class.start_handlers[prod]
      @productions << prod
      if handler
        # Create a new production data element, potentially allowing handler
        # to customize before pushing on the @prod_data stack
        progress("#{prod}(:start):#{@prod_data.length}") {@prod_data.last}
        data = {}
        begin
          self.class.eval_with_binding(self) {
            handler.call(@prod_data.last, data, @parse_callback)
          }
        rescue ArgumentError, Error => e
          error("start", "#{e.class}: #{e.message}", production: prod)
          @recovering = false
        end
        @prod_data << data
      elsif [:merge, :star].include?(@cleanup[prod])
        # Save current data to merge later
        @prod_data << {}
        progress("#{prod}(:start}:#{@prod_data.length}:cleanup:#{@cleanup[prod]}") { get_token.inspect + (@recovering ? ' recovering' : '')}
      else
        # Make sure we push as many was we pop, even if there is no
        # explicit start handler
        @prod_data << {} if self.class.production_handlers[prod]
        progress("#{prod}(:start:#{@prod_data.length})") { get_token.inspect + (@recovering ? ' recovering' : '')}
      end
      #puts "prod_data(s): " + @prod_data.inspect
    end

    # Finish of production
    def onFinish
      #puts "prod_data(f): " + @prod_data.inspect
      prod = @productions.last
      handler = self.class.production_handlers[prod]
      if handler && !@recovering
        # Pop production data element from stack, potentially allowing handler to use it
        data = @prod_data.pop
        begin
          self.class.eval_with_binding(self) {
            handler.call(@prod_data.last, data, @parse_callback)
          }
        rescue ArgumentError, Error => e
          error("finish", "#{e.class}: #{e.message}", production: prod)
          @recovering = false
        end
        progress("#{prod}(:finish):#{@prod_data.length}") {@prod_data.last}
      elsif [:merge, :star].include?(@cleanup[prod])
        data = @prod_data.pop
        input = @prod_data.last

        # Append every element in data to last prod_data
        data.each do |k, v|
          input[k] = case input[k]
          when nil then v.is_a?(Hash) ? v : Array(v)
          when Hash then input[k].merge!(v)
          else Array(input[k]) + Array(v)
          end
        end
        progress("#{prod}(:finish):#{@prod_data.length} cleanup:#{@cleanup[prod]}") {@prod_data.last}
      else
        progress("#{prod}(:finish):#{@prod_data.length}") { "recovering" if @recovering }
      end
      @productions.pop
    end

    # A terminal
    def onTerminal(prod, token)
      unless @productions.empty?
        parentProd = @productions.last
        handler = self.class.terminal_handlers[prod]
        # Allows catch-all for simple string terminals
        handler ||= self.class.terminal_handlers[nil] if prod.is_a?(String)
        if handler
          begin
            self.class.eval_with_binding(self) {
              handler.call(parentProd, token, @prod_data.last, @parse_callback)
            }
          rescue ArgumentError, Error => e
            error("terminal", "#{e.class}: #{e.message}", token: token, production: prod)
            @recovering = false
          end
          progress("#{prod}(:terminal)", "", depth: (depth + 1)) {"#{token}: #{@prod_data.last}"}
        else
          progress("#{prod}(:terminal)", "", depth: (depth + 1)) {token.to_s}
        end
      else
        error("#{parentProd}(:terminal)", "Terminal has no parent production", token: token, production: prod)
      end
    end

    ##
    # Does first include the specified token
    #
    # @param [Symbol] production
    # @param [Symbol, Lexer::Token] token
    #   A terminal, or symbol or string
    # @return [Boolean]
    def first_include?(production, token)
      if token.is_a?(Lexer::Token)
        @first.fetch(production, []).any? {|t| token === t}
      else
        @first.fetch(production, []).include?(token)
      end
    end

    ##
    # Does follow include the specified terminal
    #
    # @param [Symbol] production
    # @param [Symbol, Lexer::Token] token
    #   A terminal, or symbol or string
    # @return [Boolean]
    def follow_include?(production, token)
      if token.is_a?(Lexer::Token)
        @follow.fetch(production, []).any? {|t| token === t}
      else
        @follow.fetch(production, []).include?(token)
      end
    end

    ##
    # Return the next token, raising an error if the token is invalid
    #
    # @param [:recover] recover
    #   Recover from errors and go until next valid token or end of file
    # @return [Token]
    # @raise [Lexer::Error]
    def get_token(recover = nil)
      token = @lexer.first
      #progress("token") {token.inspect}
      @lineno = token.lineno if token
      token
    rescue Lexer::Error => e
      if recover
        # Recover from lexer error so that we can not bail out too early
        @lexer.recover
        error("get_token", "With input '#{e.input}': #{e.message}}", token: e.token, lineno: e.lineno)
        retry
      end
      raise
    end

    ##
    # Accept the first token in the input stream if it matches
    # `type\_or\_value`. Raise Error, otherwise.
    #
    # @param  [Symbol, String] type_or_value
    # @return [Token]
    # @raise [Error, Lexer::Error]
    def accept(type_or_value)
      if (token = get_token) && token === type_or_value
        debug("accept") {"#{token.inspect} === #{type_or_value.inspect}"}
        @lexer.shift
      end
    end
  public

    ##
    # Raised for errors during parsing.
    #
    # @example Raising a parser error
    #   raise Error.new(
    #     "invalid token '%' on line 10",
    #     token: '%', lineno: 9, production: :turtleDoc)
    #
    # @see https://ruby-doc.org/core/classes/StandardError.html
    class Error < StandardError
      ##
      # The current production.
      #
      # @return [Symbol]
      attr_reader :production

      ##
      # The invalid token which triggered the error.
      #
      # @return [String]
      attr_reader :token

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
      # @option options [String]         :token  (nil)
      # @option options [Integer]        :lineno (nil)
      def initialize(message, **options)
        @production = options[:production]
        @token      = options[:token]
        @lineno     = options[:lineno] || (@token.lineno if @token.respond_to?(:lineno))
        super(message.to_s)
      end
    end # class Error
  end # class Parser
end # module EBNF::LL1
