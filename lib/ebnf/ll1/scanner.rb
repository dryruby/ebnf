require 'strscan'    unless defined?(StringScanner)

module EBNF::LL1
  ##
  # Overload StringScanner with file operations
  #
  # * Reloads scanner as required until EOF.
  # * Loads to a high-water and reloads when remaining size reaches a low-water.
  #
  # FIXME: Only implements the subset required by the Lexer for now.
  class Scanner < StringScanner
    HIGH_WATER = 10240
    LOW_WATER  = 2048     # Hopefully large enough to deal with long multi-line comments

    ##
    # @!attribute [r] input
    # @return [IO, StringIO]
    attr_reader :input

    ##
    # Create a scanner, from an IO or String
    #
    # @param [String, IO, #read] input
    # @param [Hash{Symbol => Object}] options
    # @option options[Integer] :high_water (HIGH_WATER)
    # @option options[Integer] :low_water (LOW_WATER)
    # @yield [string]
    # @yieldparam [String] string data read from input file
    # @yieldreturn [String] replacement read data, useful for decoding escapes.
    # @return [Scanner]
    def initialize(input, options = {}, &block)
      @block = block
      @options = options.merge(:high_water => HIGH_WATER, :low_water => LOW_WATER)

      if input.respond_to?(:read)
        @input = input
        super("")
        feed_me
      else
        super(input.to_s)
      end
    end

    ##
    # Returns the "rest" of the line, or the next line if at EOL (i.e. everything after the scan pointer).
    # If there is no more data (eos? = true), it returns "".
    #
    # @return [String]
    def rest
      feed_me
      super
    end
    
    ##
    # Attempts to skip over the given `pattern` beginning with the scan pointer.
    # If it matches, the scan pointer is advanced to the end of the match,
    # and the length of the match is returned. Otherwise, `nil` is returned.
    #
    # similar to `scan`, but without returning the matched string.
    # @param [Regexp] pattern
    def skip(pattern)
      feed_me
      super
    end
    
    ##
    # Tries to match with `pattern` at the current position.
    #
    # If there is a match, the scanner advances the "scan pointer" and returns the matched string.
    # Otherwise, the scanner returns nil.
    #
    # If the scanner begins with the multi-line start expression
    # @example
    #     s = StringScanner.new('test string')
    #     p s.scan(/\w+/)   # -> "test"
    #     p s.scan(/\w+/)   # -> nil
    #     p s.scan(/\s+/)   # -> " "
    #     p s.scan(/\w+/)   # -> "string"
    #     p s.scan(/./)     # -> nil
    #
    # @param [Regexp] pattern
    # @return [String]
    def scan(pattern)
      feed_me
      super
    end
    
  private
    # Maintain low-water mark
    def feed_me
      if rest_size < @options[:low_water] && @input && !@input.eof?
        # Read up to high-water mark ensuring we're at an end of line
        diff = @options[:high_water] - rest_size
        string = @input.read(diff)
        string << @input.gets unless @input.eof?
        string = @block.call(string) if @block
        self << string if string
      end
    end
  end
end