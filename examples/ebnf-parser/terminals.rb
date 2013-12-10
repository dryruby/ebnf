# Terminal definitions for EBNF Parser
module EBNFParserTerminals
  SYMBOL  = %r([a-zA-Z0-9_\.]+).freeze
  HEX     = %r(\#x[0-9a-fA-F]+).freeze
  CHAR    = %r((?:#{HEX})|(?:\\[\\trn\'\"\[\]\(\)\-])|[^\t\r\n\'\"\[\]\(\)\-]).freeze
  RANGE   = %r(\[(?:#{CHAR})\-(?:#{CHAR})\]).freeze
  ENUM    = %r(\[#{CHAR}+\]).freeze
  LHS     = %r((?:#{ENUM}\s*)?#{SYMBOL}\s*::=).freeze
  O_RANGE = %r(\[^(?:#{CHAR})\-(?:#{CHAR})\]).freeze
  O_ENUM  = %r(\[^#{CHAR}+\]).freeze
  STRING1 = %r("(?:#{CHAR}|['\t\[\]\(\)\-])*").freeze
  STRING2 = %r('(?:#{CHAR}|["\t\[\]\(\)\-])*').freeze
  POSTFIX = %r([?*+]).freeze
  PASS    = %r((\s|(?:(#|//)[^\n\r]*$)|(?:/\*(?:(?:\*[^/])|[^*])*\*/))+)m.freeze
end
