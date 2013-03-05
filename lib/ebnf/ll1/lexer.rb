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
  # @see http://en.wikipedia.org/wiki/Lexical_analysis
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
    }
    ESCAPE_CHAR4        = /\\u(?:[0-9A-Fa-f]{4,4})/           # \uXXXX
    ESCAPE_CHAR8        = /\\U(?:[0-9A-Fa-f]{8,8})/           # \UXXXXXXXX
    ECHAR               = /\\./                               # More liberal unescaping
    UCHAR               = /#{ESCAPE_CHAR4}|#{ESCAPE_CHAR8}/
    COMMENT             = /#.*/
    WS                  = / |\t|\r|\n/m

    ML_START            = /\'\'\'|\"\"\"/                     # Beginning of terminals that may span lines

    ##
    # @!attribute whitespace
    # @return [Regexp] defines whitespace, defaults to WS
    attr_reader :whitespace

    ##
    # @!attribute comment
    # @return [Regexp] defines single-line comment, defaults to COMMENT
    attr_reader :comment

    ##
    # Returns a copy of the given `input` string with all `\uXXXX` and
    # `\UXXXXXXXX` Unicode codepoint escape sequences replaced with their
    # unescaped UTF-8 character counterparts.
    #
    # @param  [String] string
    # @return [String]
    # @see    http://www.w3.org/TR/rdf-sparql-query/#codepointEscape
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
    # @see    http://www.w3.org/TR/rdf-sparql-query/#grammarEscapes
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
    def self.tokenize(input, terminals, options = {}, &block)
      lexer = self.new(input, terminals, options)
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
    # @option options [Regexp]                :whitespace (WS)
    #   Regular expression matching the beginning of terminals that may cross newlines
    # @option options [Regexp]                :comment (COMMENT)
    def initialize(input = nil, terminals = nil, options = {})
      @options        = options.dup
      @whitespace     = @options[:whitespace]     || WS
      @comment        = @options[:comment]        || COMMENT
      @terminals      = terminals.map do |term|
        term.is_a?(Array) ? Terminal.new(*term) : term
      end

      raise Error, "Terminal patterns not defined" unless @terminals && @terminals.length > 0

      @lineno = 1
      @scanner = Scanner.new(input) do |string|
        string.force_encoding(Encoding::UTF_8) if string.respond_to?(:force_encoding)
        string
      end
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
    # The current line number (zero-based).
    #
    # @return [Integer]
    attr_reader   :lineno

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
    # @return [Token]
    def first
      return nil unless scanner

      @first ||= begin
        {} while !scanner.eos? && skip_whitespace
        return @scanner = nil if scanner.eos?

        token = match_token

        if token.nil?
          lexme = (scanner.rest.split(/#{@whitespace}|#{@comment}/).first rescue nil) || scanner.rest
          raise Error.new("Invalid token #{lexme[0..100].inspect}",
            :input => scanner.rest[0..100], :token => lexme, :lineno => lineno)
        end

        token
      end
    rescue ArgumentError, Encoding::CompatibilityError => e
      raise Error.new(e.message,
        :input => (scanner.rest[0..100] rescue '??'), :token => lexme, :lineno => lineno)
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
    # @return [Token]
    def recover
      until scanner.eos? do
        begin
          shift
          return first
        rescue Error, ArgumentError
          # Ignore errors until something scans, or EOS.
          scanner.pos = scanner.pos + 1
        end
      end
    end
  protected

    # @return [StringScanner]
    attr_reader :scanner

    ##
    # Skip whitespace or comments, as defined through input options or defaults
    def skip_whitespace
      # skip all white space, but keep track of the current line number
      while !scanner.eos?
       if matched = scanner.scan(@whitespace)
          @lineno += matched.count("\n")
        elsif (com = scanner.scan(@comment))
        else
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
    # @return [Token]
    def match_token
      @terminals.each do |term|
        #STDERR.puts "match[#{term.type}] #{scanner.rest[0..100].inspect} against #{term.regexp.inspect}" #if term.type == :STRING_LITERAL_SINGLE_QUOTE
        if matched = scanner.scan(term.regexp)
          #STDERR.puts "  matched #{term.type.inspect}: #{matched.inspect}"
          return token(term.type, term.canonicalize(matched))
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

      # @param [Symbol, nil] type
      # @param [Regexp] regexp
      # @param [Hash{Symbol => Object}] options
      # @option options [Hash{String => String}] :map ({})
      #   A mapping from terminals, in lower-case form, to
      #   their canonical value
      # @option options [Boolean] :unescape
      #   Cause strings and codepoints to be unescaped.
      def initialize(type, regexp, options = {})
        @type, @regexp, @options = type, regexp, options
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

  protected

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
    def token(type, value, options = {})
      Token.new(type, value, options.merge(:lineno => lineno))
    end

    ##
    # Represents a lexer token.
    #
    # @example Creating a new token
    #   token = EBNF::LL1::Lexer::Token.new(:LANGTAG, "en")
    #   token.type   #=> :LANGTAG
    #   token.value  #=> "en"
    #
    # @see http://en.wikipedia.org/wiki/Lexical_analysis#Token
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
      def initialize(type, value, options = {})
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
        {:type => @type, :value => @value}
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
        to_hash.inspect
      end
    end # class Token

    ##
    # Raised for errors during lexical analysis.
    #
    # @example Raising a lexer error
    #   raise EBNF::LL1::Lexer::Error.new(
    #     "invalid token '%' on line 10",
    #     :input => query, :token => '%', :lineno => 9)
    #
    # @see http://ruby-doc.org/core/classes/StandardError.html
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
      def initialize(message, options = {})
        @input  = options[:input]
        @token  = options[:token]
        @lineno = options[:lineno]
        super(message.to_s)
      end
    end # class Error
    
    unless "".respond_to?(:force_encoding)
      # Compatibility with 1.9 Encoding
      module Encoding
        class CompatibilityError < StandardError; end
      end
    end
  end # class Lexer
end # module EBNF
