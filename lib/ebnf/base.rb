require 'strscan'

# Extended Bakus-Nour Form (EBNF), being the W3C variation is
# originaly defined in the
# [W3C XML 1.0 Spec](http://www.w3.org/TR/REC-xml/#sec-notation).
#
# This version attempts to be less strict than the strict definition
# to allow for coloquial variations (such as in the Turtle syntax).
#
# A rule takes the following form:
#     \[1\]  symbol ::= expression
#
# Comments include the content between '/*' and '*/'
#
# @see http://www.w3.org/2000/10/swap/grammar/ebnf2turtle.py
# @see http://www.w3.org/2000/10/swap/grammar/ebnf2bnf.n3
#
# Based on bnf2turtle by Dan Connolly.
#
# Motivation
# ----------
# 
# Many specifications include grammars that look formal but are not
# actually checked, by machine, against test data sets. Debugging the
# grammar in the XML specification has been a long, tedious manual
# process. Only when the loop is closed between a fully formal grammar
# and a large test data set can we be confident that we have an accurate
# specification of a language (and even then, only the syntax of the language).
# 
# 
# The grammar in the [N3 design note][] has evolved based on the original
# manual transcription into a python recursive-descent parser and
# subsequent development of test cases. Rather than maintain the grammar
# and the parser independently, our [goal] is to formalize the language
# syntax sufficiently to replace the manual implementation with one
# derived mechanically from the specification.
# 
# 
# [N3 design note]: http://www.w3.org/DesignIssues/Notation3
# 
# Related Work
# ------------
# 
# Sean Palmer's [n3p announcement][] demonstrated the feasibility of the
# approach, though that work did not cover some aspects of N3.
# 
# In development of the [SPARQL specification][], Eric Prud'hommeaux
# developed [Yacker][], which converts EBNF syntax to perl and C and C++
# yacc grammars. It includes an interactive facility for checking
# strings against the resulting grammars.
# Yosi Scharf used it in [cwm Release 1.1.0rc1][], which includes
# a SPAQRL parser that is *almost* completely mechanically generated.
# 
# The N3/turtle output from yacker is lower level than the EBNF notation
# from the XML specification; it has the ?, +, and * operators compiled
# down to pure context-free rules, obscuring the grammar
# structure. Since that transformation is straightforwardly expressed in
# semantic web rules (see [bnf-rules.n3][]), it seems best to keep the RDF
# expression of the grammar in terms of the higher level EBNF
# constructs.
# 
# [goal]: http://www.w3.org/2002/02/mid/1086902566.21030.1479.camel@dirk;list=public-cwm-bugs
# [n3p announcement]: http://lists.w3.org/Archives/Public/public-cwm-talk/2004OctDec/0029.html
# [Yacker]: http://www.w3.org/1999/02/26-modules/User/Yacker
# [SPARQL specification]: http://www.w3.org/TR/rdf-sparql-query/
# [Cwm Release 1.1.0rc1]: http://lists.w3.org/Archives/Public/public-cwm-announce/2005JulSep/0000.html
# [bnf-rules.n3]: http://www.w3.org/2000/10/swap/grammar/bnf-rules.n3
# 
# Open Issues and Future Work
# ---------------------------
# 
# The yacker output also has the terminals compiled to elaborate regular
# expressions. The best strategy for dealing with lexical tokens is not
# yet clear. Many tokens in SPARQL are case insensitive; this is not yet
# captured formally.
# 
# The schema for the EBNF vocabulary used here (``g:seq``, ``g:alt``, ...)
# is not yet published; it should be aligned with [swap/grammar/bnf][]
# and the [bnf2html.n3][] rules (and/or the style of linked XHTML grammar
# in the SPARQL and XML specificiations).
# 
# It would be interesting to corroborate the claim in the SPARQL spec
# that the grammar is LL(1) with a mechanical proof based on N3 rules.
# 
# [swap/grammar/bnf]: http://www.w3.org/2000/10/swap/grammar/bnf
# [bnf2html.n3]: http://www.w3.org/2000/10/swap/grammar/bnf2html.n3  
# 
# Background
# ----------
# 
# The [N3 Primer] by Tim Berners-Lee introduces RDF and the Semantic
# web using N3, a teaching and scribbling language. Turtle is a subset
# of N3 that maps directly to (and from) the standard XML syntax for
# RDF.
#
# [N3 Primer]: http://www.w3.org/2000/10/swap/Primer.html
# 
# @author Gregg Kellogg
module EBNF
  class Base
    include BNF
    include LL1
    include Parser

    # Abstract syntax tree from parse
    #
    # @return [Array<Rule>]
    attr_reader :ast

    # Grammar errors, or errors found genering parse tables
    #
    # @return [Array<String>]
    attr_accessor :errors

    # Parse the string or file input generating an abstract syntax tree
    # in S-Expressions (similar to SPARQL SSE)
    #
    # @param [#read, #to_s] input
    # @param [Hash{Symbol => Object}] options
    # @option options [Boolean, Array] :debug
    #   Output debug information to an array or STDOUT.
    def initialize(input, options = {})
      @options = options
      @lineno, @depth, @errors = 1, 0, []
      terminal = false
      @ast = []

      input = input.respond_to?(:read) ? input.read : input.to_s
      scanner = StringScanner.new(input)

      eachRule(scanner) do |r|
        debug("rule string") {r.inspect}
        case r
        when /^@terminals/
          # Switch mode to parsing terminals
          terminal = true
        when /^@pass\s*(.*)$/m
          # Ignore, as we can't use this for processing the EBNF grammar itself
        else
          rule = depth {ruleParts(r)}

          rule.kind = :terminal if terminal # Override after we've parsed @terminals
          rule.orig = r
          @ast << rule
        end
      end
    end

    # Iterate over each rule or terminal, except empty
    # @param [:termina, :rule] kind
    # @yield rule
    # @yieldparam [Rule] rule
    def each(kind, &block)
      ast.each {|r| block.call(r) if r.kind == kind && r.sym != :_empty}
    end

    ##
    # Write out parsed syntax string as an S-Expression
    # @return [String]
    def to_sxp
      SXP::Generator.string(ast.sort_by{|r| r.id.to_f}.map(&:for_sxp))
    end
    def to_s; to_sxp; end

    def dup
      new_obj = super
      new_obj.instance_variable_set(:@ast, @ast.dup)
      new_obj
    end

    ##
    # Find a rule given a symbol
    # @param [Symbol] sym
    # @return [Rule]
    def find_rule(sym)
      (@find ||= {})[sym] ||= ast.detect {|r| r.sym == sym}
    end

    ##
    # Write out syntax tree as Turtle
    # @param [String] prefix for language
    # @param [String] ns URI for language
    # @return [String]
    def to_ttl(prefix, ns)
      unless ast.empty?
        [
          "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.",
          "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.",
          "@prefix #{prefix}: <#{ns}>.",
          "@prefix : <#{ns}>.",
          "@prefix re: <http://www.w3.org/2000/10/swap/grammar/regex#>.",
          "@prefix g: <http://www.w3.org/2000/10/swap/grammar/ebnf#>.",
          "",
          ":language rdfs:isDefinedBy <>; g:start :#{ast.first.id}.",
          "",
        ]
      end.join("\n") +

      ast.sort.map(&:to_ttl).join("\n")
    end

    def depth
      @depth += 1
      ret = yield
      @depth -= 1
      ret
    end

    # Progress output, less than debugging
    def progress(*args)
      return unless @options[:progress] || @options[:debug]
      options = args.last.is_a?(Hash) ? args.pop : {}
      depth = options[:depth] || @depth
      args << yield if block_given?
      message = "#{args.join(': ')}"
      str = "[#{@lineno}]#{' ' * depth}#{message}"
      @options[:debug] << str if @options[:debug].is_a?(Array)
      $stderr.puts(str) if @options[:progress] || @options[:debug] == true
    end

    # Error output
    def error(*args)
      options = args.last.is_a?(Hash) ? args.pop : {}
      depth = options[:depth] || @depth
      args << yield if block_given?
      message = "#{args.join(': ')}"
      @errors << message
      str = "[#{@lineno}]#{' ' * depth}#{message}"
      @options[:debug] << str if @options[:debug].is_a?(Array)
      $stderr.puts(str)
    end

    ##
    # Progress output when debugging
    #
    # @overload debug(node, message)
    #   @param [String] node relative location in input
    #   @param [String] message ("")
    #
    # @overload debug(message)
    #   @param [String] message ("")
    #
    # @yieldreturn [String] added to message
    def debug(*args)
      return unless @options[:debug]
      options = args.last.is_a?(Hash) ? args.pop : {}
      depth = options[:depth] || @depth
      args << yield if block_given?
      message = "#{args.join(': ')}"
      str = "[#{@lineno}]#{' ' * depth}#{message}"
      @options[:debug] << str if @options[:debug].is_a?(Array)
      $stderr.puts(str) if @options[:debug] == true
    end
  end
end