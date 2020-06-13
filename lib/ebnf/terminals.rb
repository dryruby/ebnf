# Terminal definitions for the EBNF grammar
module EBNF::Terminals
  SYMBOL  = %r([a-zA-Z0-9_\.]+).freeze
  HEX     = %r(\#x[a-fA-F0-9]+).freeze
  CHAR    = %r([\u0009\u000A\u000D\u0020-\uD7FF\u{10000}-\u{10FFFF}]).freeze
  R_CHAR  = %r([\u0009\u000A\u000D\u0020-\u005C\u005E-\uD7FF\u{10000}-\u{10FFFF}]).freeze
  RANGE   = %r(\[(?:(?:#{R_CHAR})\-(?:#{R_CHAR})|(?:#{HEX})-(?:#{HEX}))\]).freeze
  ENUM_BASE = %r(\[(?:(?:#{R_CHAR})+|(?:#{HEX})+)\]).freeze
  ENUM    = %r((?:#{ENUM_BASE})(?!\s+#{SYMBOL})).freeze
  LHS     = %r(\[(?:(?:#{SYMBOL})+\]\s+)?(?:#{SYMBOL})\s*::=).freeze
  O_RANGE = %r(\[^(?:#{R_CHAR}\-#{R_CHAR})|(?:#{HEX}-#{HEX})\]).freeze
  O_ENUM  = %r(\[^(?:#{R_CHAR})+\]).freeze
  STRING1 = %r("[\u0009\u000A\u000D\u0020\u0021\u0023-\uD7FF\u{10000}-\u{10FFFF}]*").freeze
  STRING2 = %r('[\u0009\u000A\u000D\u0020-\u0026\u0028-\uD7FF\u{10000}-\u{10FFFF}]*').freeze
  POSTFIX = %r([?*+]).freeze
  PASS    = %r((\s|(?:(#[^x]|//)[^\n\r]*$)|(?:/\*(?:(?:\*[^/])|[^*])*\*/))+)m.freeze
end
