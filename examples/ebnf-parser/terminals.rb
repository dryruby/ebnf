# Terminal definitions for EBNF Parser
module EBNFParserTerminals
  SYMBOL  = %r([a-zA-Z0-9_\.]+).freeze
  HEX     = %r(\#x[0-9a-fA-F]+).freeze
  CHAR    = %r((?:#{HEX})|(?:\\[\\trn'"])|[^\t\r\n]).freeze
  R_CHAR  = %r((?:#{HEX})|(?:\\[\\trn'"])|[^\t\r\n\]]).freeze
  RANGE   = %r(\[(?:#{R_CHAR})\-(?:#{R_CHAR})\]).freeze
  ENUM    = %r(\[#{R_CHAR}+\]).freeze
  LHS     = %r((?:#{ENUM}\s*)?#{SYMBOL}\s*::=).freeze
  O_RANGE = %r(\[^(?:#{R_CHAR})\-(?:#{R_CHAR})\]).freeze
  O_ENUM  = %r(\[^#{R_CHAR}+\]).freeze
  STRING1 = %r("(?:(?:#{HEX})|(?:\\[\\trn'"])|[^\r\n"])*").freeze
  STRING2 = %r('(?:(?:#{HEX})|(?:\\[\\trn'"])|[^\r\n'])*').freeze
  POSTFIX = %r([?*+]).freeze
  PASS    = %r((\s|(?:(#|//)[^\n\r]*$)|(?:/\*(?:(?:\*[^/])|[^*])*\*/))+)m.freeze
end
