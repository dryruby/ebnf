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
    # @!attribute [r] lineno
    # @return [Integer] line number of current token
    attr_reader :lineno

    def self.included(base)
      base.extend(ClassMethods)
    end

    # DSL for creating terminals and productions
    module ClassMethods
      def start_handlers; @@start_handlers || {}; end
      def production_handlers; @@production_handlers || {}; end
      def terminal_handlers; @@terminal_handlers || {}; end
      def patterns; @@patterns || []; end

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
      def terminal(term, regexp, options = {}, &block)
        @@patterns ||= []
        # Passed in order to define evaulation sequence
        @@patterns << EBNF::LL1::Lexer::Terminal.new(term, regexp, options)
        @@terminal_handlers ||= {}
        @@terminal_handlers[term] = block if block_given?
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
        @@start_handlers ||= {}
        @@start_handlers[term] = block
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
        @@production_handlers ||= {}
        @@production_handlers[term] = block
      end

      # Evaluate a handler, delegating to the specified object.
      # This is necessary so that handlers can operate within the
      # binding context of the parser in which they're invoked.
      # @param [Object] object
      # @return [Object]
      def eval_with_binding(object)
        @@delegate = object
        object.instance_eval {yield}
      end

      private

      def method_missing(method, *args, &block)
        if @@delegate ||= nil
          @@delegate.send method, *args, &block
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
    #     # Defines a production called during different phases of parsing
    #     # with data from previous production along with data defined for the
    #     # current production
    #     #
    #     # Yield to generate a triple
    #     production :object do |parser, phase, input, current|
    #       object = current[:resource]
    #       yield :statement, RDF::Statement.new(input[:subject], input[:predicate], object)
    #     end
    #     
    #     ##
    #     # Defines the pattern for a terminal node
    #     terminal :BLANK_NODE_LABEL, %r(_:(#{PN_LOCAL})) do |parser, production, token, input|
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
    # @param [Symbol, #to_s] prod The starting production for the parser. It may be a URI from the grammar, or a symbol representing the local_name portion of the grammar URI.
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
    # @yield [context, *data]
    #   Yields for to return data to parser
    # @yieldparam [:statement, :trace] context
    #   Context for block
    # @yieldparam [Symbol] *data
    #   Data specific to the call
    # @return [EBNF::LL1::Parser]
    # @see http://cs.adelaide.edu.au/~charles/lt/Lectures/07-ErrorRecovery.pdf
    def parse(input = nil, prod = nil, options = {}, &block)
      @options = options.dup
      @branch  = options[:branch]
      @first  = options[:first] ||= {}
      @follow  = options[:follow] ||= {}
      @lexer   = input.is_a?(Lexer) ? input : Lexer.new(input, self.class.patterns, @options)
      @productions = []
      @parse_callback = block
      @recovering = false
      @error_log = []
      terminals = self.class.patterns.map(&:type)  # Get defined terminals to help with branching

      # Unrecoverable errors
      raise Error, "Branch table not defined" unless @branch && @branch.length > 0
      raise Error, "Starting production not defined" unless prod

      @prod_data = [{}]
      prod = prod.split('#').last.to_sym unless prod.is_a?(Symbol)
      todo_stack = [{:prod => prod, :terms => nil}]

      while !todo_stack.empty?
        pushed = false
        if todo_stack.last[:terms].nil?
          todo_stack.last[:terms] = []
          cur_prod = todo_stack.last[:prod]

          # Get this first valid token appropriate for the stacked productions,
          # skipping invalid tokens until either a valid token is found (from @first),
          # or a token appearing in @follow appears.
          token = skip_until_valid(todo_stack)
          
          # At this point, token is either nil, in the first set of the production,
          # or in the follow set of this production or any previous production
          debug("parse(production)") do
            "token #{token ? token.representation.inspect : 'nil'}, " + 
            "prod #{cur_prod.inspect}, " + 
            "depth #{depth}"
          end
          
          # Got an opened production
          onStart(cur_prod)
          break if token.nil?
          
          if prod_branch = @branch[cur_prod]
            @recovering = false
            sequence = prod_branch[token.representation]
            debug("parse(production)", :level => 2) do
              "token #{token.representation.inspect} " +
              "prod #{cur_prod.inspect}, " + 
              "prod_branch #{prod_branch.keys.inspect}, " +
              "sequence #{sequence.inspect}"
            end

            if sequence.nil?
              if prod_branch.has_key?(:_empty)
                debug("parse(production)", :level => 2) {"empty sequence for _empty"}
              else
                # If there is no sequence for this production, we're
                # in error recovery, and _token_ has been advanced to
                # the point where it can reasonably follow this production
              end
            end
            todo_stack.last[:terms] += sequence if sequence
          else
            # Is this a fatal error?
            error("parse(fatal?)", "No branches found for #{cur_prod.inspect}",
              :production => cur_prod, :token => token)
          end
        end
        
        debug("parse(terms)", :level => 2) {"todo #{todo_stack.last.inspect}, depth #{depth}"}
        while !todo_stack.last[:terms].to_a.empty?
          begin
            # Get the next term in this sequence
            term = todo_stack.last[:terms].shift
            debug("parse(token)") {"accept #{term.inspect}"}
            if token = accept(term)
              @recovering = false
              debug("parse(token)") {"token #{token.inspect}, term #{term.inspect}"}
              onToken(term, token)
            elsif terminals.include?(term)
              # If term is a terminal, then it is an error if token does not
              # match it
              skip_until_valid(todo_stack)
            else
              # If it's not a string (a symbol), it is a non-terminal and we push the new state
              todo_stack << {:prod => term, :terms => nil}
              debug("parse(push)", :level => 2) {"term #{term.inspect}, depth #{depth}"}
              pushed = true
              break
            end
          end
        end
        
        # After completing the last production in a sequence, pop down until we find a production
        #
        # If in recovery mode, continue popping until we find a term with a follow list
        while !pushed &&
              !todo_stack.empty? &&
              ( (terms = todo_stack.last.fetch(:terms, [])).empty? ||
                (@recovering && @follow.fetch(terms.last, []).none? {|t| (token || :_eps) == t}))
          debug("parse(pop)", :level => 2) {"todo #{todo_stack.last.inspect}, depth #{depth}"}
          if terms.empty?
            prod = todo_stack.last[:prod]
            todo_stack.pop
            onFinish
          else
            # Stop recovering when we a production which starts with the term
            debug("parse(pop)", :level => 2) {"recovery complete"}
            @recovering = false
          end
        end
      end

      error("parse(eof)", "Finished processing before end of file", :token => @lexer.first) if @lexer.first

      # Continue popping contexts off of the stack
      while !todo_stack.empty?
        debug("parse(eof)", :level => 2) {"stack #{todo_stack.last.inspect}, depth #{depth}"}
        # There can't be anything left to do, or if there is, it must be optional
        last_terms = todo_stack.last[:terms]
        if last_terms.length > 0 && last_terms.none? {|t|
          @first.has_key?(t) && @first[t].include?(:_eps)
        }
          error("parse(eof)",
            "End of input before end of production: stack #{todo_stack.last.inspect}, depth #{depth}"
          )
        end
        todo_stack.pop
        onFinish
      end
      
      # When all is said and done, raise the error log
      unless @error_log.empty?
        raise Error, @error_log.join("\n\t") 
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
        self.class.eval_with_binding(self) {
          handler.call(@prod_data.last, data, @parse_callback)
        }
        @prod_data << data
      else
        # Make sure we push as many was we pop, even if there is no
        # explicit start handler
        @prod_data << {} if self.class.production_handlers[prod]
        progress("#{prod}(:start)") { get_token.inspect}
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
        self.class.eval_with_binding(self) {
          handler.call(@prod_data.last, data, @parse_callback)
        }
        progress("#{prod}(:finish):#{@prod_data.length}") {@prod_data.last}
      else
        progress("#{prod}(:finish)", "recovering: #{@recovering.inspect}")
      end
      @productions.pop
    end

    # A token
    def onToken(prod, token)
      unless @productions.empty?
        parentProd = @productions.last
        handler = self.class.terminal_handlers[prod]
        # Allows catch-all for simple string terminals
        handler ||= self.class.terminal_handlers[nil] if prod.is_a?(String)
        if handler
          self.class.eval_with_binding(self) {
            handler.call(parentProd, token, @prod_data.last)
          }
          progress("#{prod}(:token)", "", :depth => (depth + 1)) {"#{token}: #{@prod_data.last}"}
        else
          progress("#{prod}(:token)", "", :depth => (depth + 1)) {token.to_s}
        end
      else
        error("#{parentProd}(:token)", "Token has no parent production", :production => prod)
      end
    end
    
    # Skip through the input stream until something is found that
    # is either valid based on the content of the production stack,
    # or can follow a production in the stack.
    #
    # @return [Token]
    def skip_until_valid(todo_stack)
      cur_prod = todo_stack.last[:prod]
      token = get_token
      first = @first[cur_prod] || []
      expected = @branch.fetch(cur_prod, {}).keys
      expected << :_eps if first.include?(:_eps)  # Helps when testing

      # If we've reached EOF, token is nil. This is fine, if _eof is in @follow
      return if token.nil? && @follow.fetch(cur_prod, []).include?(:_eof)

      # If this token can be used by the top production, return it
      # Otherwise, if the banch table allows empty, also return the token
      return token if !@recovering && (expected.any? {|t| (token || :_eps) === t})

      # Otherwise, it's an error condition, and skip either until
      # we find a valid token for this production, or until we find
      # something that can follow this production
      error("skip_until_valid", "expected one of #{expected.map(&:inspect).join(", ")}, found #{token.inspect}",
        :production => cur_prod, :token => token)

      debug("recovery", "stack follows:")
      todo_stack.reverse.each do |todo|
        debug("recovery") {"  #{todo[:prod]}: #{@follow[todo[:prod]].inspect}"}
      end

      # Find all follows to the top of the stack
      follows = todo_stack.inject([]) do |follow, todo|
        prod = todo[:prod]
        follow += @follow[prod] || []
      end.uniq
      debug("recovery") {"follows: #{follows.inspect}"}

      # Skip tokens until one is found in first or follows
      while (token = get_token) && (first + follows).none? {|t| token === t}
        skipped = @lexer.shift
        progress("recovery") {"skip #{skipped.inspect}"}
      end
      debug("recovery") {"found #{token.inspect} in #{first.include?(token) ? 'first' : 'follows'}"}
      
      # If the token is a first, just return it. Otherwise, it is a follow
      # and we need to skip to the end of the production
      unless first.any? {|t| token == t} || todo_stack.last[:terms].empty?
        debug("recovery") {"token in follows, skip past #{todo_stack.last[:terms].inspect}"}
        todo_stack.last[:terms] = [] 
      end
      token
    end

    ##
    # @param [String] node Relevant location associated with message
    # @param [String] message Error string
    # @param [Hash] options
    # @option options [URI, #to_s] :production
    # @option options [Token] :token
    def error(node, message, options = {})
      message += ", found #{options[:token].representation.inspect}" if options[:token]
      message += " at line #{@lineno}" if @lineno
      message += ", production = #{options[:production].inspect}" if options[:production]
      @error_log << message unless @recovering
      @recovering = true
      debug(node, message, options.merge(:level => 0))
    end

    ##
    # Return the next token, entering error recovery if the token is invalid
    #
    # @return [Token]
    def get_token
      token = begin
        @lexer.first
      rescue EBNF::LL1::Lexer::Error => e
        # Recover from lexer error
        @lineno = e.lineno
        error("get_token", "With input '#{e.input}': #{e.message}",
              :production => @productions.last)

        # Retrieve next valid token
        t = @lexer.recover
        debug("get_token", :level => 2) {"skipped to #{t.inspect}"}
        t
      end
      #progress("token") {token.inspect}
      @lineno = token.lineno if token
      token
    end

    ##
    # Progress output when parsing
    #   param [String] node Relevant location associated with message
    #   param [String] message ("")
    #   param [Hash] options
    #   option options [Integer] :depth
    #     Recursion depth for indenting output
    #   yieldreturn [String] added to message
    def progress(node, *args)
      return unless @options[:progress] || @options[:debug]
      options = args.last.is_a?(Hash) ? args.pop : {}
      message = args.join(",")
      depth = options[:depth] || self.depth
      message += yield.to_s if block_given?
      if @options[:debug]
        return debug(node, message, {:level => 0}.merge(options))
      else
        $stderr.puts("[#{@lineno}]#{' ' * depth}#{node}: #{message}")
      end
    end

    ##
    # Progress output when debugging
    # @param [String] node Relevant location associated with message
    # @param [String] message ("")
    # @param [Hash] options
    # @option options [Integer] :depth
    #   Recursion depth for indenting output
    # @yieldreturn [String] added to message
    def debug(node, message = "", options = {})
      return unless @options[:debug]
      debug_level = options.fetch(:level, 1)
      return unless debug_level <= DEBUG_LEVEL
      depth = options[:depth] || self.depth
      message += yield if block_given?
      str = "[#{@lineno}](#{debug_level})#{' ' * depth}#{node}: #{message}"
      case @options[:debug]
      when Array
        @options[:debug] << str
      when TrueClass
        $stderr.puts str
      when :yield
        @parse_callback.call(:trace, node, message, options)
      end
    end

    ##
    # Accept the first token in the input stream if it matches
    # _type\_or\_value_. Return nil otherwise.
    #
    # @param  [Symbol, String] type_or_value
    # @return [Token]
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
    #     :token => '%', :lineno => 9, :production => :turtleDoc)
    #
    # @see http://ruby-doc.org/core/classes/StandardError.html
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
      def initialize(message, options = {})
        @production = options[:production]
        @token      = options[:token]
        @lineno     = options[:lineno]
        super(message.to_s)
      end
    end # class Error
  end # class Parser
end # module EBNF::LL1
