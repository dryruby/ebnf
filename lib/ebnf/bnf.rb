module EBNF
  module BNF
    ##
    # Transform EBNF Rule set to BNF:
    #
    #   * Add rule [0] (_empty rule (seq))
    #   * Transform each rule into a set of rules that are just BNF, using {Rule#to_bnf}.
    # @return [ENBF] self
    def make_bnf
      progress("make_bnf") {"Start: #{@ast.length} rules"}
      new_ast = [Rule.new(:_empty, "0", [:seq], kind: :rule)]

      ast.each do |rule|
        debug("make_bnf") {"expand from: #{rule.inspect}"}
        new_rules = rule.to_bnf
        debug(" => ") {new_rules.map(&:sym).join(', ')}
        new_ast += new_rules
      end

      # Consolodate equivalent terminal rules
      to_rewrite = {}
      new_ast.select {|r| r.terminal?}.each do |src_rule|
        new_ast.select {|r| r.terminal?}.each do |dst_rule|
          if src_rule.equivalent?(dst_rule) && src_rule != dst_rule
            debug("make_bnf") {"equivalent rules: #{src_rule.inspect} and #{dst_rule.inspect}"}
            (to_rewrite[src_rule] ||= []) << dst_rule
          end
        end
      end

      # Replace references to equivalent rules with canonical rule
      to_rewrite.each do |src_rule, dst_rules|
        dst_rules.each do |dst_rule|
          new_ast.each do |mod_rule|
            debug("make_bnf") {"rewrite #{mod_rule.inspect} from #{dst_rule.sym} to #{src_rule.sym}"}
            mod_rule.rewrite(dst_rule, src_rule)
          end
        end
      end

      # AST now has just rewritten rules
      compacted_ast = new_ast - to_rewrite.values.flatten.compact

      # Sort AST by number
      @ast = compacted_ast
      progress("make_bnf") {"End: #{@ast.length} rules"}
      self
    end
  end
end
