require 'strscan'
require 'sxp' unless defined?(SXP)

# Extended Bakus-Nour Form (EBNF), being the W3C variation is
# originaly defined in the
# [W3C XML 1.0 Spec](https://www.w3.org/TR/REC-xml/#sec-notation).
#
# This version attempts to be less strict than the strict definition
# to allow for coloquial variations (such as in the Turtle syntax).
#
# A rule takes the following form:
#     \[1\]  symbol ::= expression
#
# Comments include the content between '/*' and '*/'
#
# @see https://www.w3.org/2000/10/swap/grammar/ebnf2turtle.py
# @see https://www.w3.org/2000/10/swap/grammar/ebnf2bnf.n3
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
# [N3 design note]: https://www.w3.org/DesignIssues/Notation3
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
# [goal]: https://www.w3.org/2002/02/mid/1086902566.21030.1479.camel@dirk;list=public-cwm-bugs
# [n3p announcement]: https://lists.w3.org/Archives/Public/public-cwm-talk/2004OctDec/0029.html
# [Yacker]: https://rubygems/02/26-modules/User/Yacker
# [SPARQL specification]: https://www.w3.org/TR/rdf-sparql-query/
# [Cwm Release 1.1.0rc1]: https://lists.w3.org/Archives/Public/public-cwm-announce/2005JulSep/0000.html
# [bnf-rules.n3]: https://www.w3.org/2000/10/swap/grammar/bnf-rules.n3
# 
# [swap/grammar/bnf]: https://www.w3.org/2000/10/swap/grammar/bnf
# [bnf2html.n3]: https://www.w3.org/2000/10/swap/grammar/bnf2html.n3  
# 
# Background
# ----------
# 
# The [N3 Primer] by Tim Berners-Lee introduces RDF and the Semantic
# web using N3, a teaching and scribbling language. Turtle is a subset
# of N3 that maps directly to (and from) the standard XML syntax for
# RDF.
#
# [N3 Primer]: https://www.w3.org/2000/10/swap/Primer.html
# 
# @author Gregg Kellogg
module EBNF
  class Base
    include BNF
    include LL1
    include Native
    include PEG

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
    # @param [Symbol] format (:ebnf)
    #   Format of input, one of `:abnf`, `:ebnf`, `:isoebnf`, `:isoebnf`, `:native`, or `:sxp`.
    #   Use `:native` for the native EBNF parser, rather than the PEG parser.
    # @param [Hash{Symbol => Object}] options
    # @option options [Boolean] :level
    #   Trace level. 0(debug), 1(info), 2(warn), 3(error).
    # @option options [Boolean, Array] :validate
    #   Validate resulting grammar.
    def initialize(input, format: :ebnf, **options)
      @options = options.dup
      @lineno, @depth, @errors = 1, 0, []
      @ast = []

      input = input.respond_to?(:read) ? input.read : input.to_s

      case format
      when :abnf
        abnf = ABNF.new(input, **options)
        @ast = abnf.ast
      when :ebnf
        ebnf = Parser.new(input, **options)
        @ast = ebnf.ast
      when :isoebnf
        iso = ISOEBNF.new(input, **options)
        @ast = iso.ast
      when :native
        terminals = false
        scanner = StringScanner.new(input)

        eachRule(scanner) do |r|
          debug("rule string") {r.inspect}
          case r
          when /^@terminals/
            # Switch mode to parsing terminals
            terminals = true
            rule = Rule.new(nil, nil, nil, kind: :terminals, ebnf: self)
            @ast << rule
          when /^@pass\s*(.*)$/m
            expr = expression($1).first
            rule = Rule.new(nil, nil, expr, kind: :pass, ebnf: self)
            rule.orig = expr
            @ast << rule
          else
            rule = depth {ruleParts(r)}

            rule.kind = :terminal if terminals # Override after we've parsed @terminals
            rule.orig = r
            @ast << rule
          end
        end
      when :sxp
        @ast = SXP::Reader::Basic.read(input).map {|e| Rule.from_sxp(e)}
      else
        raise "unknown input format #{format.inspect}"
      end

      validate! if @options[:validate]
    end

    ##
    # Validate the grammar.
    #
    # Makes sure that rules reference either strings or other defined rules.
    #
    # @raise [RangeError]
    def validate!
      ast.each do |rule|
        begin
          rule.validate!(@ast)
        rescue SyntaxError => e
          error("In rule #{rule.sym}: #{e.message}")
        end
      end
      raise SyntaxError, errors.join("\n") unless errors.empty?
    end

    ##
    # Is the grammar valid?
    #
    # Uses `#validate!` and catches `RangeError`
    #
    # @return [Boolean]
    def valid?
      validate!
      true
    rescue SyntaxError
      false
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
    #
    # @return [String]
    def to_sxp(**options)
      require 'sxp' unless defined?(SXP)
      SXP::Generator.string(ast.map(&:for_sxp))
    end

    ##
    # Output formatted EBNF
    #
    # @param [:abnf, :ebnf, :isoebnf] format (:ebnf)
    # @return [String]
    def to_s(format: :ebnf)
      Writer.string(*ast, format: format)
    end

    ##
    # Output formatted EBNF as HTML
    #
    # @param [:abnf, :ebnf, :isoebnf] format (:ebnf)
    # @param [Boolean] validate (false) validate generated HTML.
    # @return [String]
    def to_html(format: :ebnf, validate: false)
      Writer.html(*ast, format: format, validate: validate)
    end

    ##
    # Output Ruby parser files
    #
    # @param [IO, StringIO] output
    # @param [String] grammarFile
    # @param [String] mod_name ('Meta')
    def to_ruby(output = $stdout, grammarFile: nil, mod_name: 'Meta', **options)
      unless output == $stdout
        output.puts "# This file is automatically generated by ebnf version #{EBNF::VERSION}"
        output.puts "# Derived from #{grammarFile}" if grammarFile
        unless self.errors.empty?
          output.puts "# Note, grammar has errors, may need to be resolved manually:"
          #output.puts "#   #{pp.conflicts.map{|c| c.join("\n#      ")}.join("\n#   ")}"
        end
        output.puts "module #{mod_name}"
        output.puts "  START = #{self.start.inspect}\n" if self.start
      end

      # Either output LL(1) BRANCH tables or rules for PEG parsing
      if ast.first.first
        to_ruby_ll1(output)
      else
        to_ruby_peg(output)
      end
      unless output == $stdout
        output.puts "end"
      end
    end

    ##
    # Renumber, rule identifiers
    def renumber!
      ast.each_with_index do |rule, index|
        rule.id = (index + 1).to_s
      end
    end

    ##
    # Write out syntax tree as Turtle
    # @param [String] prefix for language
    # @param [String] ns URI for language
    # @return [String]
    def to_ttl(prefix = nil, ns = "http://example.org/")
      unless ast.empty?
        [
          "@prefix dc: <http://purl.org/dc/terms/>.",
          "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.",
          "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.",
          ("@prefix #{prefix}: <#{ns}>." if prefix),
          "@prefix : <#{ns}>.",
          "@prefix re: <http://www.w3.org/2000/10/swap/grammar/regex#>.",
          "@prefix g: <http://www.w3.org/2000/10/swap/grammar/ebnf#>.",
          "",
          ":language rdfs:isDefinedBy <>; g:start :#{ast.first.id}.",
          "",
        ].compact
      end.join("\n") +

      ast.map(&:to_ttl).join("\n")
    end

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

    def depth
      @depth += 1
      ret = yield
      @depth -= 1
      ret
    end

    # Progress output, less than debugging
    def progress(*args, **options)
      debug(*args, **options.merge(level: Logger::INFO))
    end

    # Error output
    def error(*args, **options)
      depth = options[:depth] || @depth
      args << yield if block_given?
      message = "#{args.join(': ')}"
      debug(message, **options.merge(level: Logger::ERROR))
      @errors << message
      $stderr.puts(message)
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
    def debug(*args, **options)
      return unless @options.key?(:logger)
      level = @options[:level] || Logger::DEBUG
      depth = options[:depth] || @depth
      args << yield if block_given?
      message = "#{args.join(': ')}"
      str = "[#{@lineno}]#{' ' * depth}#{message}"
      if @options[:logger].respond_to?(:add)
        @options[:logger].add(level, str)
      elsif @options[:logger].respond_to?(:<<)
        @options[:logger] << "[#{lineno}] " + str
      end
    end
  end
end