# -*- encoding: utf-8 -*-
require 'rdf'

##
# Serialize ruleset back to EBNF
module EBNF
  class Writer
    LINE_LENGTH = 80

    ##
    # Format rules to a String
    #
    # @param  [Array<Rule>] rules
    # @return [Object]
    def self.string(*rules)
      require 'stringio' unless defined?(StringIO)
      buf = StringIO.new
      write(buf, *rules)
      buf.string
    end

    ##
    # Format rules to $stdout
    #
    # @param  [Array<Rule>] rules
    # @return [Object]
    def self.print(*rules)
      write($stdout, *rules)
    end

    ##
    # Write formatted rules to an IO like object
    #
    # @param  [Object] out
    # @param  [Array<Rule>] rules
    # @return [Object]
    def self.write(out, *rules)
      Writer.new(rules, out: out)
    end

    ##
    # @param [Array<Rule>] rules
    # @param [Hash{Symbol => Object}] options
    # @option options [Symbol] :format
    # @option options [#write] :out ($stdout)
    def initialize(rules, options = {})
      out = options.fetch(:out, $stdio)
      #fmt = options.fetch(:format, :ebnf)

      # Determine max LHS length
      max_id = rules.max_by {|r| r.id.to_s.length}.id.to_s.length
      max_sym = rules.max_by {|r| r.sym.to_s.length}.sym.to_s.length
      lhs_length = max_sym + 3
      lhs_fmt = "%-#{max_sym}{sym} ::= "
      if max_id > 0
        lhs_fmt = "%-#{max_id+2}{id} " + lhs_fmt
        lhs_length += max_id + 3
      end
      rhs_length = LINE_LENGTH - lhs_length

      # Format each rule, considering the available rhs size
      rules.each do |rule|
        buffer = if rule.pass?
          "%-#{lhs_length-2}s" % "@pass"
        else
          lhs_fmt % {:id => "[#{rule.id}]", :sym => rule.sym}
        end
        formatted_expr = format(rule.expr)
        if formatted_expr.length > rhs_length
          buffer << format(rule.expr, ("\n" + " " * lhs_length))
        else
          buffer << formatted_expr
        end
        out.puts(buffer)
      end
    end

    protected
    # Format the expression part of a rule
    def format(expr, sep = nil)
      return expr.to_s if expr.is_a?(Symbol)
      return %("#{escape(expr)}") if expr.is_a?(String)

      case expr.first
      when :alt, :diff
        this_sep = (sep ? sep : " ") + {alt: "| ", diff: "- "}[expr.first.to_sym]
        expr[1..-1].map {|e| format(e)}.join(this_sep)
      when :star, :plus, :opt
        raise "Expected star expression to have a single operand" unless expr.length == 2
        char = {star: "*", plus: "+", opt: "?"}[expr.first.to_sym]
        r = format(expr[1])
        (r.start_with?("(") || Array(expr[1]).length == 1) ? "#{r}#{char}" : "(#{r})#{char}"
      when :range
        parts = expr.last.split(/(?!\\)-/, 2)
        "[" + parts.map {|e| format(e)[1..-2]}.join("-") + "]"
      when :seq
        this_sep = (sep ? sep : " ")
        expr[1..-1].map {|e| r = format(e); Array(e).length > 2 ? "(#{r})" : r}.join(this_sep)
      else
        raise "Unknown operator: #{expr.first}"
      end
    end

    def escape(string)
      buffer = ""
      string.each_char do |c|
        buffer << case c.to_s
        when "\t" then "\\t"
        when "\n" then "\\n"
        when "\r" then "\\r"
        when "\\" then "\\\\"
        #when "("  then "\\("
        #when ")"  then "\\)"
        #when "["  then "\\["
        #when "]"  then "\\]"
        #when "-"  then "\\\\-"
        when "'"  then "\\'"
        when '"'  then "\\\""
        else c
        end
      end
      buffer
    end
  end
end
