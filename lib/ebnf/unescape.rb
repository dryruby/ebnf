# encoding: utf-8
# Unsecape strings
module EBNF::Unescape
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
  UCHAR               = /#{ESCAPE_CHAR4}|#{ESCAPE_CHAR8}/n.freeze

  ##
  # Returns a copy of the given `input` string with all `\uXXXX` and
  # `\UXXXXXXXX` Unicode codepoint escape sequences replaced with their
  # unescaped UTF-8 character counterparts.
  #
  # @param  [String] string
  # @return [String]
  # @see    https://www.w3.org/TR/rdf-sparql-query/#codepointEscape
  def unescape_codepoints(string)
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
  module_function :unescape_codepoints

  ##
  # Returns a copy of the given `input` string with all string escape
  # sequences (e.g. `\n` and `\t`) replaced with their unescaped UTF-8
  # character counterparts.
  #
  # @param  [String] input
  # @return [String]
  # @see    https://www.w3.org/TR/rdf-sparql-query/#grammarEscapes
  def unescape_string(input)
    input.gsub(ECHAR) {|escaped| ESCAPE_CHARS[escaped] || escaped}
  end
  module_function :unescape_string

  # Perform string and codepoint unescaping if defined for this terminal
  # @param [String] string
  # @return [String]
  def unescape(string)
    unescape_string(unescape_codepoints(string))
  end
  module_function :unescape
end