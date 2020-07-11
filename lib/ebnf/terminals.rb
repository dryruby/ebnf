# encoding: utf-8
# Terminal definitions for the EBNF grammar
module EBNF::Terminals
  SYMBOL_BASE  = %r(\b[a-zA-Z0-9_\.]+\b)u.freeze
  SYMBOL  = %r(#{SYMBOL_BASE}(?!\s*::=))u.freeze
  HEX     = %r(\#x\h+)u.freeze
  CHAR    = %r([\u0009\u000A\u000D\u0020-\uD7FF\u{10000}-\u{10FFFF}])u.freeze
  R_CHAR  = %r([\u0009\u000A\u000D\u0020-\u002C\u002E-\u005C\u005E-\uD7FF\u{10000}-\u{10FFFF}])u.freeze
  RANGE   = %r(\[(?:(?:#{R_CHAR}\-#{R_CHAR})|(?:#{HEX}\-#{HEX})|#{R_CHAR}|#{HEX})+-?\](?!\s+#{SYMBOL_BASE}\s*::=))u.freeze
  LHS     = %r((?:\[#{SYMBOL_BASE}\])?\s*#{SYMBOL_BASE}\s*::=)u.freeze
  O_RANGE = %r(\[^(?:(?:#{R_CHAR}\-#{R_CHAR})|(?:#{HEX}\-#{HEX}|#{R_CHAR}|#{HEX}))+-?\])u.freeze
  STRING1 = %r("[\u0009\u000A\u000D\u0020\u0021\u0023-\uD7FF\u{10000}-\u{10FFFF}]*")u.freeze
  STRING2 = %r('[\u0009\u000A\u000D\u0020-\u0026\u0028-\uD7FF\u{10000}-\u{10FFFF}]*')u.freeze
  POSTFIX = %r([?*+])u.freeze
  PASS    = %r((
    \s
  | (?:(?:\#[^x]|//)[^\n\r]*)
  | (?:/\*(?:(?:\*[^/])|[^*])*\*/)
  | (?:\(\*(?:(?:\*[^\)])|[^*])*\*\))
  )+)xmu.freeze
end
