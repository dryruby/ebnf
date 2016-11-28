# coding: utf-8
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
    HIGH_WATER = 512 * 1024     # Hopefully large enough to deal with long multi-line comments
    LOW_WATER  = 4 * 1024

    ##
    # @return [IO, StringIO]
    attr_reader :input

    ##
    # If we don't have an IO input, simply use StringScanner directly
    # @private
    def self.new(input, options = {})
      if input.respond_to?(:read)
        scanner = self.allocate
        scanner.send(:initialize, input, options)
      else
        if input.encoding != Encoding::UTF_8
          input = input.dup if input.frozen?
          input.force_encoding(Encoding::UTF_8)
        end
        StringScanner.new(input)
      end
    end

    ##
    # Create a scanner, from an IO
    #
    # @param [String, IO, #read] input
    # @param [Hash{Symbol => Object}] options
    # @option options[Integer] :high_water (HIGH_WATER)
    # @option options[Integer] :low_water (LOW_WATER)
    # @return [Scanner]
    def initialize(input, options = {})
      @options = options.merge(high_water: HIGH_WATER, low_water: LOW_WATER)

      @input = input
      super("")
      feed_me
      self
    end

    ##
    # Returns the "rest" of the line, or the next line if at EOL (i.e. everything after the scan pointer).
    # If there is no more data (eos? = true), it returns "".
    #
    # @return [String]
    def rest
      feed_me
      encode_utf8 super
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
    # Returns true if the scan pointer is at the end of the string
    #
    # @return [Boolean]
    def eos?
      feed_me
      super
    end

    ##
    # Set the scan pointer to the end of the string and clear matching data
    def terminate
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
      encode_utf8 super
    end

    ##
    # Ensures that the input buffer is full to the high water mark, or end of file. Useful when matching tokens that may be longer than the low water mark
    def ensure_buffer_full
      # Read up to high-water mark ensuring we're at an end of line
      if @input && !@input.eof?
        diff = @options[:high_water] - rest_size
        string = encode_utf8(@input.read(diff))
        string << encode_utf8(@input.gets) unless @input.eof?
        self << string if string
      end
    end

  private
    # Maintain low-water mark
    def feed_me
      ensure_buffer_full if rest_size < @options[:low_water]
    end

    # Perform UTF-8 encoding of input
    def encode_utf8(string)
      if string && string.encoding != Encoding::UTF_8
        string = string.dup if string.frozen?
        string.force_encoding(Encoding::UTF_8)
      end
      string
    end
  end
end