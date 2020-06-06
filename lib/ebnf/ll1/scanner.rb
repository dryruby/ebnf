# coding: utf-8
require 'strscan'    unless defined?(StringScanner)

module EBNF::LL1
  ##
  # Overload StringScanner with file operations and line counting
  #
  # * Reloads scanner as required until EOF.
  # * Loads to a high-water and reloads when remaining size reaches a low-water.
  #
  # FIXME: Only implements the subset required by the Lexer for now.
  class Scanner < StringScanner
    HIGH_WATER = 512 * 1024     # Hopefully large enough to deal with long multi-line comments
    LOW_WATER  = 4 * 1024

    ##
    # @return [String, IO, StringIO]
    attr_reader :input

    ##
    # The current line number (one-based).
    #
    # @return [Integer]
    attr_accessor   :lineno

    ##
    # Create a scanner, from an IO
    #
    # @param [String, IO, #read] input
    # @param [Hash{Symbol => Object}] options
    # @option options[Integer] :high_water (HIGH_WATER)
    # @option options[Integer] :low_water (LOW_WATER)
    # @return [Scanner]
    def initialize(input, **options)
      @options = options.merge(high_water: HIGH_WATER, low_water: LOW_WATER)

      @previous_lineno = @lineno = 1
      @input = input
      super(input.is_a?(String) ? input : "")
      feed_me
      self
    end

    ##
    # Ensures that the input buffer is full to the high water mark, or end of file. Useful when matching tokens that may be longer than the low water mark
    def ensure_buffer_full
      # Read up to high-water mark ensuring we're at an end of line
      if @input.respond_to?(:eof?) && !@input.eof?
        diff = @options[:high_water] - rest_size
        string = encode_utf8(@input.read(diff))
        string << encode_utf8(@input.gets) unless @input.eof?
        self << string if string
      end
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
    # Returns the "rest" of the line, or the next line if at EOL (i.e. everything after the scan pointer).
    # If there is no more data (eos? = true), it returns "".
    #
    # @return [String]
    def rest
      feed_me
      @lineno += 1 if eos?
      encode_utf8 super
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
      @previous_lineno = @lineno
      if matched = encode_utf8(super)
        @lineno += matched.count("\n")
      end
      matched
    end

    ##
    # Scans the string until the pattern is matched. Returns the substring up to and including the end of the match, advancing the scan pointer to that location. If there is no match, nil is returned.
    #
    # @example
    #     s = StringScanner.new("Fri Dec 12 1975 14:39")
    #     s.scan_until(/1/)        # -> "Fri Dec 1"
    #     s.pre_match              # -> "Fri Dec "
    #     s.scan_until(/XYZ/)      # -> nil
    #
    # @param [Regexp] pattern
    # @return [String]
    def scan_until(pattern)
      feed_me
      @previous_lineno = @lineno
      if matched = encode_utf8(super)
        @lineno += matched.count("\n")
      end
      matched
    end

    ##
    # Attempts to skip over the given `pattern` beginning with the scan pointer.
    # If it matches, the scan pointer is advanced to the end of the match,
    # and the length of the match is returned. Otherwise, `nil` is returned.
    #
    # similar to `scan`, but without returning the matched string.
    # @param [Regexp] pattern
    def skip(pattern)
      scan(pattern)
      nil
    end

    ##
    # Advances the scan pointer until pattern is matched and consumed. Returns the number of bytes advanced, or nil if no match was found.
    #
    # Look ahead to match pattern, and advance the scan pointer to the end of the match. Return the number of characters advanced, or nil if the match was unsuccessful.
    #
    # It’s similar to scan_until, but without returning the intervening string.
    # @param [Regexp] pattern
    def skip_until(pattern)
      (matched = scan_until(pattern)) && matched.length
    end

    ##
    # Sets the scan pointer to the previous position. Only one previous position is remembered, and it changes with each scanning operation.
    def unscan
      @lineno = @previous_lineno
      super
    end

    ##
    # Set the scan pointer to the end of the string and clear matching data
    def terminate
      feed_me
      super
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