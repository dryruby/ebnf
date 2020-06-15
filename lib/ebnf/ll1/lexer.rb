module EBNF::LL1
  require 'ebnf/ll1/scanner'    unless defined?(Scanner)

  ##
  # A lexical analyzer
  #
  # @example Tokenizing a Turtle string
  #   terminals = [
  #     [:BLANK_NODE_LABEL, %r(_:(#{PN_LOCAL}))],
  #     ...
  #   ]
  #   ttl = "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ."
  #   lexer = EBNF::LL1::Lexer.tokenize(ttl, terminals)
  #   lexer.each_token do |token|
  #     puts token.inspect
  #   end
  #
  # @example Tokenizing and returning a token stream
  #   lexer = EBNF::LL1::Lexer.tokenize(...)
  #   while :some-condition
  #     token = lexer.first # Get the current token
  #     token = lexer.shift # Get the current token and shift to the next
  #   end
  #
  # @example Handling error conditions
  #   begin
  #     EBNF::LL1::Lexer.tokenize(query)
  #   rescue EBNF::LL1::Lexer::Error => error
  #     warn error.inspect
  #   end
  #
  # @see https://en.wikipedia.org/wiki/Lexical_analysis
  class Lexer
    include Enumerable

    ESCAPE_CHARS         = {
      '\\t'   => "\t",  # \u0009 (tab)
      '\\n'   => "\n",  # \u000A (line feed)
      '\\r'   => "\r",  # \u000D (carriage return)
      '\\b'   => "\b",  # \u0008 (backspace)
      '\\f'   => "\f",  # \u000C (form feed)
      '\\"'  => '"',    # \u0022 (quotation mark, double quote mark)
      "\\'"  => '\'',   # \u0027 (apostrophe-quote, single quote mark)
      '\\\\' => '\\'    # \u005C (backslash)
    }.freeze
    ESCAPE_CHAR4        = /\\u(?:[0-9A-Fa-f]{4,4})/u.freeze    # \uXXXX
    ESCAPE_CHAR8        = /\\U(?:[0-9A-Fa-f]{8,8})/u.freeze    # \UXXXXXXXX
    ECHAR               = /\\./u.freeze                        # More liberal unescaping
    UCHAR               = /#{ESCAPE_CHAR4}|#{ESCAPE_CHAR8}/u.freeze

    ##
    # @return [Regexp] defines whitespace, including comments, otherwise whitespace must be explicit in terminals
    attr_reader :whitespace

    ##
    # Returns a copy of the given `input` string with all `\uXXXX` and
    # `\UXXXXXXXX` Unicode codepoint escape sequences replaced with their
    # unescaped UTF-8 character counterparts.
    #
    # @param  [String] string
    # @return [String]
    # @see    https://www.w3.org/TR/rdf-sparql-query/#codepointEscape
    def self.unescape_codepoints(string)
      string = string.dup
      string.force_encoding(Encoding::ASCII_8BIT) if string.respond_to?(:force_encoding)

      # Decode \uXXXX and \UXXXXXXXX code points:
      string = string.gsub(UCHAR) do |c|
        s = [(c[2..-1]).hex].pack('U*')
        s.respond_to?(:force_encoding) ? s.force_encoding(Encoding::ASCII_8BIT) : s
      end

      string.force_encoding(Encoding::UTF_8) if string.respond_to?(:force_encoding) 
      string
    end

    ##
    # Returns a copy of the given `input` string with all string escape
    # sequences (e.g. `\n` and `\t`) replaced with their unescaped UTF-8
    # character counterparts.
    #
    # @param  [String] input
    # @return [String]
    # @see    https://www.w3.org/TR/rdf-sparql-query/#grammarEscapes
    def self.unescape_string(input)
      input.gsub(ECHAR) { |escaped| ESCAPE_CHARS[escaped] || escaped[1..-1]}
    end

    ##
    # Tokenizes the given `input` string or stream.
    #
    # @param  [String, #to_s]                 input
    # @param  [Array<Array<Symbol, Regexp>>]  terminals
    #   Array of symbol, regexp pairs used to match terminals.
    #   If the symbol is nil, it defines a Regexp to match string terminals.
    # @param  [Hash{Symbol => Object}]        options
    # @yield  [lexer]
    # @yieldparam [Lexer] lexer
    # @return [Lexer]
    # @raise  [Lexer::Error] on invalid input
    def self.tokenize(input, terminals, **options, &block)
      lexer = self.new(input, terminals, **options)
      block_given? ? block.call(lexer) : lexer
    end

    ##
    # Initializes a new lexer instance.
    #
    # @param  [String, #to_s]                 input
    # @param  [Array<Array<Symbol, Regexp>, Terminal>]  terminals
    #   Array of symbol, regexp pairs used to match terminals.
    #   If the symbol is nil, it defines a Regexp to match string terminals.
    # @param  [Hash{Symbol => Object}]        options
    # @option options [Regexp]                :whitespace
    #   Whitespace between tokens, including comments
    # @option options[Integer] :high_water passed to scanner
    # @option options[Integer] :low_water passed to scanner
    def initialize(input = nil, terminals = nil, **options)
      @options        = options.dup
      @whitespace     = @options[:whitespace]
      @terminals      = terminals.map do |term|
        if term.is_a?(Array) && term.length ==3
          # Last element is options
          Terminal.new(term[0], term[1], **term[2])
        elsif term.is_a?(Array)
          Terminal.new(*term)
        else
          term
        end
      end

      raise Error, "Terminal patterns not defined" unless @terminals && @terminals.length > 0

      @scanner = Scanner.new(input, **options)
    end

    ##
    # Any additional options for the lexer.
    #
    # @return [Hash]
    attr_reader   :options

    ##
    # The current input string being processed.
    #
    # @return [String]
    attr_accessor :input

    ##
    # Returns `true` if the input string is lexically valid.
    #
    # To be considered valid, the input string must contain more than zero
    # terminals, and must not contain any invalid terminals.
    #
    # @return [Boolean]
    def valid?
      begin
        !count.zero?
      rescue Error
        false
      end
    end

    ##
    # Enumerates each token in the input string.
    #
    # @yield  [token]
    # @yieldparam [Token] token
    # @return [Enumerator]
    def each_token(&block)
      if block_given?
        while token = shift
          yield token
        end
      end
      enum_for(:each_token)
    end
    alias_method :each, :each_token

    ##
    # Returns first token in input stream
    #
    # @param [Array[Symbol]] types Optional set of types for restricting terminals examined
    # @return [Token]
    def first(*types)
      return nil unless scanner

      @first ||= begin
        {} while !scanner.eos? && skip_whitespace
        return nil if scanner.eos?

        token = match_token(*types)

        if token.nil?
          lexme = (scanner.rest.split(@whitespace || /\s/).first rescue nil) || scanner.rest
          raise Error.new("Invalid token #{lexme[0..100].inspect}",
            input: scanner.rest[0..100], token: lexme, lineno: lineno)
        end

        token
      end
    rescue ArgumentError, Encoding::CompatibilityError => e
      raise Error.new(e.message,
        input: (scanner.rest[0..100] rescue '??'), token: lexme, lineno: lineno)
    rescue Error
      raise
    rescue
      STDERR.puts "Expected ArgumentError, got #{$!.class}"
      raise
    end

    ##
    # Returns first token and shifts to next
    #
    # @return [Token]
    def shift
      cur = first
      @first = nil
      cur
    end
    
    ##
    # Skip input until a token is matched
    #
    # @param [Array[Symbol]] types Optional set of types for restricting terminals examined
    # @return [Token]
    def recover(*types)
       until scanner.eos? || tok = match_token(*types)
        if scanner.skip_until(@whitespace || /\s+/m).nil? # Skip past current "token"
          # No whitespace at the end, must be and end of string
          scanner.terminate
        else
          skip_whitespace
        end
      end
      scanner.unscan if tok
      first
    end

    ##
    # The current line number (one-based).
    #
    # @return [Integer]
    def lineno
      scanner.lineno
    end
  protected

    # @return [StringScanner]
    attr_reader :scanner

    ##
    # Skip whitespace, as defined through input options or defaults
    def skip_whitespace
      # skip all white space, but keep track of the current line number
      while @whitespace && !scanner.eos?
        unless scanner.scan(@whitespace)
          return
        end
      end
    end

    ##
    # Return the matched token.
    #
    # If the token was matched with a case-insensitive regexp,
    # track this with the resulting {Token}, so that comparisons
    # with that token are also case insensitive
    #
    # @param [Array[Symbol]] types Optional set of types for restricting terminals examined
    # @return [Token]
    def match_token(*types)
      @terminals.each do |term|
        next unless types.empty? || types.include?(term.type)
        #STDERR.puts "match[#{term.type}] #{scanner.rest[0..100].inspect} against #{term.regexp.inspect}" #if term.type == :STRING_LITERAL_SINGLE_QUOTE
        if term.partial_regexp && scanner.match?(term.partial_regexp) && !scanner.match?(term.regexp) && scanner.respond_to?(:ensure_buffer_full)
          scanner.ensure_buffer_full
        end

        if matched = scanner.scan(term.regexp)
          #STDERR.puts "  matched #{term.type.inspect}: #{matched.inspect}"
          tok = token(term.type, term.canonicalize(matched))
          return tok
        end
      end
      nil
    end

    # Terminal class, representing the terminal identifier and
    # matching regular expression. Optionally, a Terminal may include
    # a map to turn case-insensitively matched terminals into their
    # canonical form
    class Terminal
      attr_reader :type
      attr_reader :regexp
      attr_reader :partial_regexp

      # @param [Symbol, nil] type
      # @param [Regexp] regexp
      # @param [Hash{Symbol => Object}] options
      # @option options [Hash{String => String}] :map ({})
      #   A mapping from terminals, in lower-case form, to
      #   their canonical value
      # @option options [Boolean] :unescape
      #   Cause strings and codepoints to be unescaped.
      # @option options [Regexp] :partial_regexp
      #   A regular expression matching the beginning of this terminal; useful for terminals that match things longer than the scanner low water mark.
      def initialize(type, regexp, **options)
        @type, @regexp, @options = type, regexp, options
        @partial_regexp = options[:partial_regexp]
        @map = options.fetch(:map, {})
      end
      
      # Map a terminal to it's canonical form. If there is no
      # map, `value` is returned. `value` is unescaped if there
      # is no canonical mapping, and the `:unescape` option is set.
      #
      # @param [String] value
      #   value to canonicalize
      # @return [String]
      def canonicalize(value)
        @map.fetch(value.downcase, unescape(value))
      end

      def ==(other)
        case other
        when Array
          @type == other.first && @regexp == other.last
        when Terminal
          @type == other.type && @regexp == other.regexp
        end
      end

      protected

      # Perform string and codepoint unescaping if defined for this terminal
      # @param [String] string
      # @return [String]
      def unescape(string)
        if @options[:unescape]
          Lexer.unescape_string(Lexer.unescape_codepoints(string))
        else
          string
        end
      end

    end

    ##
    # Constructs a new token object annotated with the current line number.
    #
    # The parser relies on the type being a symbolized URI and the value being
    # a string, if there is no type. If there is a type, then the value takes
    # on the native representation appropriate for that type.
    #
    # @param  [Symbol] type
    # @param  [String] value
    #   Scanner instance with access to matched groups
    # @param  [Hash{Symbol => Object}] options
    # @return [Token]
    def token(type, value, **options)
      Token.new(type, value, lineno: lineno, **options)
    end

    ##
    # Represents a lexer token.
    #
    # @example Creating a new token
    #   token = EBNF::LL1::Lexer::Token.new(:LANGTAG, "en")
    #   token.type   #=> :LANGTAG
    #   token.value  #=> "en"
    #
    # @see https://en.wikipedia.org/wiki/Lexical_analysis#Token
    class Token
      ##
      # The token's symbol type.
      #
      # @return [Symbol]
      attr_reader :type

      ##
      # The token's value.
      #
      # @return [String]
      attr_reader :value

      ##
      # The line number where the token was encountered.
      #
      # @return [Integer]
      attr_reader :lineno

      ##
      # Any additional options for the token.
      #
      # @return [Hash]
      attr_reader :options

      ##
      # Initializes a new token instance.
      #
      # @param  [Symbol]                 type
      # @param  [String]                 value
      # @param  [Hash{Symbol => Object}] options
      # @option options [Integer]        :lineno (nil)
      def initialize(type, value, **options)
        @type = type.to_s.to_sym if type
        @value = value.to_s
        @options = options.dup
        @lineno  = @options.delete(:lineno)
      end

      ##
      # Returns the attribute named by `key`.
      #
      # @param  [Symbol] key
      # @return [Object]
      def [](key)
        key = key.to_s.to_sym unless key.is_a?(Integer) || key.is_a?(Symbol)
        case key
          when 0, :type    then @type
          when 1, :value   then @value
          else nil
        end
      end

      ##
      # Returns `true` if the given `value` matches either the type or value
      # of this token.
      #
      # @example Matching using the symbolic type
      #   EBNF::LL1::Lexer::Token.new(:NIL) === :NIL     #=> true
      #
      # @example Matching using the string value
      #   EBNF::LL1::Lexer::Token.new(nil, "{") === "{"  #=> true
      #
      # @param  [Symbol, String] value
      # @return [Boolean]
      def ===(value)
        case value
          when Symbol
            value == @type
          when ::String
            @value == (@options[:case_insensitive] ? value.to_s.downcase : value.to_s)
          else value == @value
        end
      end

      ##
      # Returns a hash table representation of this token.
      #
      # @return [Hash]
      def to_hash
        {type: @type, value: @value}
      end
      
      ##
      # Readable version of token
      def to_s
        @type ? @type.inspect : @value
      end

      ##
      # Returns type, if not nil, otherwise value
      def representation
        @type ? @type : @value
      end

      ##
      # Returns an array representation of this token.
      #
      # @return [Array]
      def to_a
        [@type, @value]
      end

      ##
      # Returns a developer-friendly representation of this token.
      #
      # @return [String]
      def inspect
        "#{@value.inspect}#{'(' + @type.to_s + ')' if @type}"
      end
    end # class Token

    ##
    # Raised for errors during lexical analysis.
    #
    # @example Raising a lexer error
    #   raise EBNF::LL1::Lexer::Error.new(
    #     "invalid token '%' on line 10",
    #     input: query, token: '%', lineno: 9)
    #
    # @see https://ruby-doc.org/core/classes/StandardError.html
    class Error < StandardError
      ##
      # The input string associated with the error.
      #
      # @return [String]
      attr_reader :input

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
      # @option options [String]         :input  (nil)
      # @option options [String]         :token  (nil)
      # @option options [Integer]        :lineno (nil)
      def initialize(message, **options)
        @input  = options[:input]
        @token  = options[:token]
        @lineno = options[:lineno]
        super(message.to_s)
      end
    end # class Error
  end # class Lexer
end # module EBNF
