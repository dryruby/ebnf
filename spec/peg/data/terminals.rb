# Terminal definitions for EBNF Parser
module EBNFPegParserTerminals
  SYMBOL  = %r([a-zA-Z0-9_\.]+).freeze
  HEX     = %r(\#x[0-9a-fA-F]+).freeze
  CHAR    = %r((?:#{HEX})|(?:\\[\\trn'"])|[^\t\r\n]).freeze
  R_CHAR  = %r((?:#{HEX})|(?:\\[\\trn'"])|[^\t\r\n\]]).freeze
  RANGE   = %r(\[(?:#{R_CHAR})\-(?:#{R_CHAR})\]).freeze
  ENUM_BASE = %r(\[#{R_CHAR}+\]).freeze
  ENUM    = %r(#{ENUM_BASE}(?!\s+#{SYMBOL})).freeze  # Not followed by symbol
  LHS     = %r((?:#{ENUM_BASE}\s*)?#{SYMBOL}\s*::=).freeze
  O_RANGE = %r(\[^(?:#{R_CHAR})\-(?:#{R_CHAR})\]).freeze
  O_ENUM  = %r(\[^#{R_CHAR}+\]).freeze
  STRING1 = %r("(?:(?:#{HEX})|(?:\\[\\trn'"])|[^\r\n"])*").freeze
  STRING2 = %r('(?:(?:#{HEX})|(?:\\[\\trn'"])|[^\r\n'])*').freeze
  POSTFIX = %r([?*+]).freeze
  PASS    = %r((\s|(?:(#[^x]|//)[^\n\r]*$)|(?:/\*(?:(?:\*[^/])|[^*])*\*/))+)m.freeze
end
