# encoding: utf-8
# Terminal definitions for the EBNF grammar
module EBNF::Terminals
  SYMBOL  = %r([a-zA-Z0-9_\.]+)u.freeze
  HEX     = %r(\#x[a-fA-F0-9]+)u.freeze
  CHAR    = %r([\u0009\u000A\u000D\u0020-\uD7FF\u{10000}-\u{10FFFF}])u.freeze
  R_CHAR  = %r([\u0009\u000A\u000D\u0020-\u005C\u005E-\uD7FF\u{10000}-\u{10FFFF}])u.freeze
  RANGE   = %r(\[(?:(?:#{R_CHAR})\-(?:#{R_CHAR})|(?:#{HEX})-(?:#{HEX}))\])u.freeze
  ENUM_BASE = %r(\[(?:(?:#{R_CHAR})+|(?:#{HEX})+)\])u.freeze
  ENUM    = %r((?:#{ENUM_BASE})(?!\s+#{SYMBOL}\s*::=))u.freeze
  LHS     = %r(\[(?:(?:#{SYMBOL})+\]\s+)?(?:#{SYMBOL})\s*::=)u.freeze
  O_RANGE = %r(\[^(?:#{R_CHAR}\-#{R_CHAR})|(?:#{HEX}-#{HEX})\])u.freeze
  O_ENUM  = %r(\[^(?:#{R_CHAR})+\])u.freeze
  STRING1 = %r("[\u0009\u000A\u000D\u0020\u0021\u0023-\uD7FF\u{10000}-\u{10FFFF}]*")u.freeze
  STRING2 = %r('[\u0009\u000A\u000D\u0020-\u0026\u0028-\uD7FF\u{10000}-\u{10FFFF}]*')u.freeze
  POSTFIX = %r([?*+])u.freeze
  PASS    = %r((\s|(?:(#[^x]|//)[^\n\r]*$)|(?:/\*(?:(?:\*[^/])|[^*])*\*/))+)mu.freeze
end
