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

      @ast = new_ast
      progress("make_bnf") {"End: #{@ast.length} rules"}
      self
    end
  end
end
