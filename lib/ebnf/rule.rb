require 'scanf'
require 'strscan'

module EBNF
  # Represent individual parsed rules
  class Rule
    # Operations which are flattened to seprate rules in to_bnf.
    BNF_OPS = %w{
      alt diff not opt plus rept seq star
    }.map(&:to_sym).freeze

    TERM_OPS = %w{
      hex istr range
    }.map(&:to_sym).freeze

    # The number of arguments expected per operator. `nil` for unspecified
    OP_ARGN =       {
      alt: nil,
      diff: 2,
      hex: 1,
      istr: 1,
      not: 1,
      opt: 1,
      plus: 1,
      range: 1,
      rept: 3,
      seq: nil,
      star: 1
    }

    # Symbol of rule
    #
    # @return [Symbol]
    attr_accessor :sym

    # ID of rule
    # @return [String]
    attr_accessor :id

    # A comprehension is a sequence which contains all elements but the first of the original rule.
    #
    # @return [Rule]
    attr_accessor :comp

    # Kind of rule
    #
    # @return [:rule, :terminal, :terminals, or :pass]
    attr_accessor :kind

    # Rule expression
    #
    # @return [Array]
    attr_accessor :expr

    # Original EBNF
    #
    # @return [String]
    attr_accessor :orig

    # Terminals that immediately procede this rule
    #
    # @return [Array<Rule>]
    attr_reader :first

    # Terminals that immediately follow this rule
    #
    # @return [Array<Rule>]
    attr_reader :follow

    # Indicates that this is a starting rule
    #
    # @return [Boolean]
    attr_accessor :start

    # Determines preparation and cleanup rules for reconstituting EBNF ? * + from BNF
    attr_accessor :cleanup

    # @param [Symbol, nil] sym
    #   `nil` is allowed only for @pass or @terminals
    # @param [Integer, nil] id
    # @param [Array] expr
    #   The expression is an internal-representation of an S-Expression with one of the following oparators:
    #
    #   * `alt` – A list of alternative rules, which are attempted in order. It terminates with the first matching rule, or is terminated as unmatched, if no such rule is found.
    #   * `diff` – matches any string that matches `A` but does not match `B`.
    #   * `hex` – A single character represented using the hexadecimal notation `#xnn`.
    #   * `istr` – A string which matches in a case-insensitive manner, so that `(istr "fOo")` will match either of the strings `"foo"`, `"FOO"` or any other combination.
    #   * `opt` – An optional rule or terminal. It either results in the matching rule or returns `nil`.
    #   * `plus` – A sequence of one or more of the matching rule. If there is no such rule, it is terminated as unmatched; otherwise, the result is an array containing all matched input.
    #   * `range` – A range of characters, possibly repeated, of the form `(range "a-z")`. May also use hexadecimal notation.
    #   * `rept m n` – A sequence of at lest `m` and at most `n` of the matching rule. It will always return an array.
    #   * `seq` – A sequence of rules or terminals. If any (other than `opt` or `star`) to not parse, the rule is terminated as unmatched.
    #   * `star` – A sequence of zero or more of the matching rule. It will always return an array.
    # @param [:rule, :terminal, :terminals, :pass] kind (nil)
    # @param [String] ebnf (nil)
    #   When parsing, records the EBNF string used to create the rule.
    # @param [Array] first (nil)
    #   Recorded set of terminals that can proceed this rule (LL(1))
    # @param [Array] follow (nil)
    #   Recorded set of terminals that can follow this rule (LL(1))
    # @param [Boolean] start (nil)
    #   Is this the starting rule for the grammar?
    # @param [Rule] top_rule (nil)
    #   The top-most rule. All expressed rules are top-rules, derived rules have the original rule as their top-rule.
    # @param [Boolean] cleanup (nil)
    #   Records information useful for cleaning up converted :plus, and :star expansions (LL(1)).
    def initialize(sym, id, expr, kind: nil, ebnf: nil, first: nil, follow: nil, start: nil, top_rule: nil, cleanup: nil)
      @sym, @id = sym, id
      @expr = expr.is_a?(Array) ? expr : [:seq, expr].compact
      @ebnf, @kind, @first, @follow, @start, @cleanup, @top_rule = ebnf, kind, first, follow, start, cleanup, top_rule
      @top_rule ||= self
      @kind ||= case
      when sym.to_s == sym.to_s.upcase then :terminal
      when !BNF_OPS.include?(@expr.first) then :terminal
      else :rule
      end

      # Allow @pass and @terminals to not be named
      @sym ||= :_pass if @kind == :pass
      @sym ||= :_terminals if @kind == :terminals

      raise ArgumentError, "Rule sym must be a symbol, was #{@sym.inspect}" unless @sym.is_a?(Symbol)
      raise ArgumentError, "Rule id must be a string or nil, was #{@id.inspect}" unless (@id || "").is_a?(String)
      raise ArgumentError, "Rule kind must be one of :rule, :terminal, :terminals, or :pass, was #{@kind.inspect}" unless
        @kind.is_a?(Symbol) && %w(rule terminal terminals pass).map(&:to_sym).include?(@kind)

      case @expr.first
      when :alt
        raise ArgumentError, "#{@expr.first} operation must have at least one operand, had #{@expr.length - 1}" unless @expr.length > 1
      when :diff
        raise ArgumentError, "#{@expr.first} operation must have exactly two operands, had #{@expr.length - 1}" unless @expr.length == 3
      when :hex, :istr, :not, :opt, :plus, :range, :star
        raise ArgumentError, "#{@expr.first} operation must have exactly one operand, had #{@expr.length - 1}" unless @expr.length == 2
      when :rept
        raise ArgumentError, "#{@expr.first} operation must have exactly three, had #{@expr.length - 1}" unless @expr.length == 4
        raise ArgumentError, "#{@expr.first} operation must an non-negative integer minimum, was #{@expr[1]}" unless
          @expr[1].is_a?(Integer) && @expr[1] >= 0
        raise ArgumentError, "#{@expr.first} operation must an non-negative integer maximum or '*', was #{@expr[2]}" unless
          @expr[2] == '*' || @expr[2].is_a?(Integer) && @expr[2] >= 0
      when :seq
        # It's legal to have a zero-length sequence
      else
        raise ArgumentError, "Rule expression must be an array using a known operator, was #{@expr.first}"
      end
    end

    ##
    # Return a rule from its SXP representation:
    #
    # @example inputs
    #    (pass _pass (plus (range "#x20\\t\\r\\n")))
    #    (rule ebnf "1" (star (alt declaration rule)))
    #    (terminal R_CHAR "19" (diff CHAR (alt "]" "-")))
    #
    # Also may have `(first ...)`, `(follow ...)`, or `(start #t)`.
    #
    # @param [String, Array] sxp
    # @return [Rule]
    def self.from_sxp(sxp)
      if sxp.is_a?(String)
        require 'sxp' unless defined?(SXP)
        sxp = SXP.parse(sxp)
      end
      expr = sxp.detect {|e| e.is_a?(Array) && ![:first, :follow, :start].include?(e.first.to_sym)}
      first = sxp.detect {|e| e.is_a?(Array) && e.first.to_sym == :first}
      first = first[1..-1] if first
      follow = sxp.detect {|e| e.is_a?(Array) && e.first.to_sym == :follow}
      follow = follow[1..-1] if follow
      cleanup = sxp.detect {|e| e.is_a?(Array) && e.first.to_sym == :cleanup}
      cleanup = cleanup[1..-1] if cleanup
      start = sxp.any? {|e| e.is_a?(Array) && e.first.to_sym == :start}
      sym = sxp[1] if sxp[1].is_a?(Symbol)
      id = sxp[2] if sxp[2].is_a?(String)
      self.new(sym, id, expr, kind: sxp.first, first: first, follow: follow, cleanup: cleanup, start: start)
    end

    # Build a new rule creating a symbol and numbering from the current rule
    # Symbol and number creation is handled by the top-most rule in such a chain.
    #
    # @param [Array] expr
    # @param [Symbol] kind (nil)
    # @param [Hash{Symbol => Symbol}] cleanup (nil)
    # @param [Hash{Symbol => Object}] options
    def build(expr, kind: nil, cleanup: nil, **options)
      new_sym, new_id = @top_rule.send(:make_sym_id)
      self.class.new(new_sym, new_id, expr,
                     kind: kind,
                     ebnf: @ebnf,
                     top_rule: @top_rule,
                     cleanup: cleanup,
                     **options)
    end

    # Return representation for building S-Expressions.
    #
    # @return [Array]
    def for_sxp
      elements = [kind, sym]
      elements << id if id
      elements << [:start, true] if start
      elements << first.sort_by(&:to_s).unshift(:first) if first
      elements << follow.sort_by(&:to_s).unshift(:follow) if follow
      elements << [:cleanup, cleanup] if cleanup
      elements << expr
      elements
    end

    # Return SXP representation of this rule
    #
    # @return [String]
    def to_sxp(**options)
      require 'sxp' unless defined?(SXP)
      for_sxp.to_sxp(**options)
    end

    alias_method :to_s, :to_sxp
    
    # Serializes this rule to an Turtle.
    #
    # @return [String]
    def to_ttl
      @ebnf.debug("to_ttl") {inspect} if @ebnf
      statements = [%{:#{sym} rdfs:label "#{sym}";}]
      if orig
        comment = orig.to_s.strip.
          gsub(/"""/, '\"\"\"').
          gsub("\\", "\\\\").
          sub(/^\"/, '\"').
          sub(/\"$/m, '\"')
        statements << %{  rdfs:comment #{comment.inspect};}
      end
      statements << %{  dc:identifier "#{id}";} if id
      
      statements += ttl_expr(expr, terminal? ? "re" : "g", 1, false)
      "\n" + statements.join("\n")
    end

    # Return a Ruby representation of this rule
    # @return [String]
    def to_ruby
      "EBNF::Rule.new(#{sym.inspect}, #{id.inspect}, #{expr.inspect}#{', kind: ' + kind.inspect unless kind == :rule})"
    end

    ##
    # Transform EBNF rule to BNF rules:
    #
    #   * Transform `(rule a "n" (op1 (op2)))` into two rules:
    #
    #         (rule a "n" (op1 _a_1))
    #         (rule _a_1 "n.1" (op2))
    #   * Transform `(rule a (opt b))` into `(rule a (alt _empty b))`
    #   * Transform `(rule a (star b))` into `(rule a (alt _empty (seq b a)))`
    #   * Transform `(rule a (plus b))` into `(rule a (seq b (star b)`
    #
    # Transformation includes information used to re-construct non-transformed.
    #
    # AST representation
    # @return [Array<Rule>]
    def to_bnf
      return [self] unless rule?
      new_rules = []

      # Look for rules containing recursive definition and rewrite to multiple rules. If `expr` contains elements which are in array form, where the first element of that array is a symbol, create a new rule for it.
      if expr.any? {|e| e.is_a?(Array) && (BNF_OPS + TERM_OPS).include?(e.first)}
        #   * Transform (a [n] rule (op1 (op2))) into two rules:
        #     (a.1 [n.1] rule (op1 a.2))
        #     (a.2 [n.2] rule (op2))
        # duplicate ourselves for rewriting
        this = dup
        new_rules << this

        expr.each_with_index do |e, index|
          next unless e.is_a?(Array) && e.first.is_a?(Symbol)
          new_rule = build(e)
          this.expr[index] = new_rule.sym
          new_rules << new_rule
        end

        # Return new rules after recursively applying #to_bnf
        new_rules = new_rules.map {|r| r.to_bnf}.flatten
      elsif expr.first == :opt
        this = dup
        #   * Transform (rule a (opt b)) into (rule a (alt _empty b))
        this.expr = [:alt, :_empty, expr.last]
        this.cleanup = :opt
        new_rules = this.to_bnf
      elsif expr.first == :star
        #   * Transform (rule a (star b)) into (rule a (alt _empty (seq b a)))
        this = dup
        this.cleanup = :star
        new_rule = this.build([:seq, expr.last, this.sym], cleanup: :merge)
        this.expr = [:alt, :_empty, new_rule.sym]
        new_rules = [this] + new_rule.to_bnf
      elsif expr.first == :plus
        #   * Transform (rule a (plus b)) into (rule a (seq b (star b)
        this = dup
        this.cleanup = :plus
        this.expr = [:seq, expr.last, [:star, expr.last]]
        new_rules = this.to_bnf
      elsif [:alt, :seq].include?(expr.first)
        # Otherwise, no further transformation necessary
        new_rules << self
      elsif [:diff, :hex, :range].include?(expr.first)
        # This rules are fine, they just need to be terminals
        raise "Encountered #{expr.first.inspect}, which is a #{self.kind}, not :terminal" unless self.terminal?
        new_rules << self
      else
        # Some case we didn't think of
        raise "Error trying to transform #{expr.inspect} to BNF"
      end
      
      return new_rules
    end

    ##
    # Transform EBNF rule for PEG:
    #
    #   * Transform `(rule a "n" (op1 ... (op2 y) ...z))` into two rules:
    #
    #         (rule a "n" (op1 ... _a_1 ... z))
    #         (rule _a_1 "n.1" (op2 y))
    #   * Transform `(rule a "n" (diff op1 op2))` into two rules:
    #
    #         (rule a "n" (seq _a_1 op1))
    #         (rule _a_1 "n.1" (not op1))
    #
    # @return [Array<Rule>]
    def to_peg
      new_rules = []

      # Look for rules containing sub-sequences
      if expr.any? {|e| e.is_a?(Array) && e.first.is_a?(Symbol)}
        # duplicate ourselves for rewriting
        this = dup
        new_rules << this

        expr.each_with_index do |e, index|
          next unless e.is_a?(Array) && e.first.is_a?(Symbol)
          new_rule = build(e)
          this.expr[index] = new_rule.sym
          new_rules << new_rule
        end

        # Return new rules after recursively applying #to_bnf
        new_rules = new_rules.map {|r| r.to_peg}.flatten
      elsif expr.first == :diff && !terminal?
        this = dup
        new_rule = build([:not, expr[2]])
        this.expr = [:seq, new_rule.sym, expr[1]]
        new_rules << this
        new_rules << new_rule
      elsif [:hex, :istr, :range].include?(expr.first)
        # This rules are fine, they just need to be terminals
        raise "Encountered #{expr.first.inspect}, which is a #{self.kind}, not :terminal" unless self.terminal?
        new_rules << self
      else
        new_rules << self
      end
      
      return new_rules.map {|r| r.extend(EBNF::PEG::Rule)}
    end

    ##
    # For :hex or :range, create a regular expression.
    #
    # @return [Regexp]
    def to_regexp
      case expr.first
      when :hex
        Regexp.new(Regexp.escape(translate_codepoints(expr[1])))
      when :istr
        /#{expr.last}/ui
      when :range
        Regexp.new("[#{escape_regexp_character_range(translate_codepoints(expr[1]))}]")
      else
        raise "Can't turn #{expr.inspect} into a regexp"
      end
    end

    # Is this a terminal?
    # 
    # @return [Boolean]
    def terminal?
      kind == :terminal
    end

    # Is this a pass?
    # @return [Boolean]
    def pass?
      kind == :pass
    end

    # Is this a rule?
    # @return [Boolean]
    def rule?
      kind == :rule
    end

    # Is this rule of the form (alt ...)?
    def alt?
      expr.is_a?(Array) && expr.first == :alt
    end

    # Is this rule of the form (seq ...)?
    def seq?
      expr.is_a?(Array) && expr.first == :seq
    end

    def inspect
      "#<EBNF::Rule:#{object_id} " +
      {sym: sym, id: id, kind: kind, expr: expr}.inspect +
      ">"
    end

    # Two rules are equal if they have the same {#sym}, {#kind} and {#expr}.
    #
    # @param [Rule] other
    # @return [Boolean]
    def ==(other)
      other.is_a?(Rule) &&
      sym   == other.sym &&
      kind  == other.kind &&
      expr  == other.expr
    end

    # Two rules are equivalent if they have the same {#expr}.
    #
    # @param [Rule] other
    # @return [Boolean]
    def eql?(other)
      expr == other.expr
    end

    # Rules compare using their ids
    def <=>(other)
      if id && other.id
        if id == other.id
          id.to_s <=> other.id.to_s
        else
          id.to_f <=> other.id.to_f
        end
      else
        sym.to_s <=> other.sym.to_s
      end
    end

    ##
    # Utility function to translate code points of the form '#xN' into ruby unicode characters
    def translate_codepoints(str)
      str.gsub(/#x\h+/) {|c| c[2..-1].scanf("%x").first.chr(Encoding::UTF_8)}
    end

    # Return the non-terminals for this rule.
    #
    # * `alt` => this is every non-terminal.
    # * `diff` => this is every non-terminal.
    # * `hex` => nil
    # * `istr` => nil
    # * `not` => this is the last expression, if any.
    # * `opt` => this is the last expression, if any.
    # * `plus` => this is the last expression, if any.
    # * `range` => nil
    # * `rept` => this is the last expression, if any.
    # * `seq` => this is the first expression in the sequence, if any.
    # * `star` => this is the last expression, if any.
    #
    # @param [Array<Rule>] ast
    #   The set of rules, used to turn symbols into rules
    # @param [Array<Symbol,String,Array>] expr (@expr)
    #   The expression to check, defaults to the rule expression.
    #   Typically, if the expression is recursive, the embedded expression is called recursively.
    # @return [Array<Rule>]
    # @note this is used for LL(1) tansformation, so rule types are limited
    def non_terminals(ast, expr = @expr)
      ([:alt, :diff].include?(expr.first) ? expr[1..-1] : expr[1,1]).map do |sym|
        case sym
        when Symbol
          r = ast.detect {|r| r.sym == sym}
          r if r && r.rule?
        when Array
          non_terminals(ast, sym)
        else
          nil
        end
      end.flatten.compact.uniq
    end

    # Return the terminals for this rule.
    #
    # * `alt` => this is every terminal.
    # * `diff` => this is every terminal.
    # * `hex` => nil
    # * `istr` => nil
    # * `not` => this is the last expression, if any.
    # * `opt` => this is the last expression, if any.
    # * `plus` => this is the last expression, if any.
    # * `range` => nil
    # * `rept` => this is the last expression, if any.
    # * `seq` => this is the first expression in the sequence, if any.
    # * `star` => this is the last expression, if any.
    #
    # @param [Array<Rule>] ast
    #   The set of rules, used to turn symbols into rules
    # @param [Array<Symbol,String,Array>] expr (@expr)
    #   The expression to check, defaults to the rule expression.
    #   Typically, if the expression is recursive, the embedded expression is called recursively.
    # @return [Array<Rule>]
    # @note this is used for LL(1) tansformation, so rule types are limited
    def terminals(ast, expr = @expr)
      ([:alt, :diff].include?(expr.first) ? expr[1..-1] : expr[1,1]).map do |sym|
        case sym
        when Symbol
          r = ast.detect {|r| r.sym == sym}
          r if r && r.terminal?
        when String
          sym
        when Array
          terminals(ast, sym)
        end
      end.flatten.compact.uniq
    end

    # Return the symbols used in the rule.
    #
    # @param [Array<Symbol,String,Array>] expr (@expr)
    #   The expression to check, defaults to the rule expression.
    #   Typically, if the expression is recursive, the embedded expression is called recursively.
    # @return [Array<Rule>]
    def symbols(expr = @expr)
      expr[1..-1].map do |sym|
        case sym
        when Symbol
          sym
        when Array
          symbols(sym)
        end
      end.flatten.compact.uniq
    end

    ##
    # The following are used for LL(1) transformation.
    ##

    # Does this rule start with `sym`? It does if expr is that sym,
    # expr starts with alt and contains that sym,
    # or expr starts with seq and the next element is that sym.
    #
    # @param [Symbol, class] sym
    #   Symbol matching any start element, or if it is String, any start element which is a String
    # @return [Array<Symbol, String>] list of symbol (singular), or strings which are start symbol, or nil if there are none
    def starts_with?(sym)
      if seq? && sym === (v = expr.fetch(1, nil))
        [v]
      elsif alt? && expr.any? {|e| sym === e}
        expr.select {|e| sym === e}
      else
        nil
      end
    end

    ##
    # Validate the rule, with respect to an AST.
    #
    # @param [Array<Rule>] ast
    #   The set of rules, used to turn symbols into rules
    # @param [Array<Symbol,String,Array>] expr (@expr)
    #   The expression to check, defaults to the rule expression.
    #   Typically, if the expression is recursive, the embedded expression is called recursively.
    # @raise [RangeError]
    def validate!(ast, expr = @expr)
      op = expr.first
      raise SyntaxError, "Unknown operator: #{op}" unless OP_ARGN.key?(op)
      raise SyntaxError, "Argument count missmatch on operator #{op}, had #{expr.length - 1} expected #{OP_ARGN[op]}" if
        OP_ARGN[op] && OP_ARGN[op] != expr.length - 1

      # rept operator needs min and max
      if op == :alt
        raise SyntaxError, "alt operation must have at least one operand, had #{expr.length - 1}" unless expr.length > 1
      elsif op == :rept
        raise SyntaxError, "rept operation must an non-negative integer minimum, was #{expr[1]}" unless
          expr[1].is_a?(Integer) && expr[1] >= 0
        raise SyntaxError, "rept operation must an non-negative integer maximum or '*', was #{expr[2]}" unless
          expr[2] == '*' || expr[2].is_a?(Integer) && expr[2] >= 0
      end

      case op
      when :hex
        raise SyntaxError, "Hex operand must be of form '#xN+': #{sym}" unless expr.last.match?(/^#x\h+$/)
      when :range
        str = expr.last.dup
        str = str[1..-1] if str.start_with?('^')
        str = str[0..-2] if str.end_with?('-')  # Allowed at end of range
        scanner = StringScanner.new(str)
        hex = rchar = in_range = false
        while !scanner.eos?
          begin
            if scanner.scan(Terminals::HEX)
              raise SyntaxError if in_range && rchar
              rchar = in_range = false
              hex = true
            elsif scanner.scan(Terminals::R_CHAR)
              raise SyntaxError if in_range && hex
              hex = in_range = false
              rchar = true
            else
              raise(SyntaxError, "Range contains illegal components at offset #{scanner.pos}: was #{expr.last}")
            end

            if scanner.scan(/\-/)
              raise SyntaxError if in_range
              in_range = true
            end
          rescue SyntaxError
            raise(SyntaxError, "Range contains illegal components at offset #{scanner.pos}: was #{expr.last}")
          end
        end
      else
        ([:alt, :diff].include?(expr.first) ? expr[1..-1] : expr[1,1]).each do |sym|
          case sym
          when Symbol
            r = ast.detect {|r| r.sym == sym}
            raise SyntaxError, "No rule found for #{sym}" unless r
          when Array
            validate!(ast, sym)
          when String
            raise SyntaxError, "String must be of the form CHAR*" unless sym.match?(/^#{Terminals::CHAR}*$/)
          end
        end
      end
    end

    ##
    # Validate the rule, with respect to an AST.
    #
    # Uses `#validate!` and catches `RangeError`
    #
    # @param [Array<Rule>] ast
    #   The set of rules, used to turn symbols into rules
    # @return [Boolean]
    def valid?(ast)
      validate!(ast)
      true
    rescue SyntaxError
      false
    end

    # Do the firsts of this rule include the empty string?
    #
    # @return [Boolean]
    def first_includes_eps?
      @first && @first.include?(:_eps)
    end

    # Add terminal as proceding this rule.
    #
    # @param [Array<Rule, Symbol, String>] terminals
    # @return [Integer] if number of terminals added
    def add_first(terminals)
      @first ||= []
      terminals = terminals.map {|t| t.is_a?(Rule) ? t.sym : t} - @first
      @first += terminals
      terminals.length
    end

    # Add terminal as following this rule. Don't add _eps as a follow
    #
    # @param [Array<Rule, Symbol, String>] terminals
    # @return [Integer] if number of terminals added
    def add_follow(terminals)
      # Remove terminals already in follows, and empty string
      terminals = terminals.map {|t| t.is_a?(Rule) ? t.sym : t} - (@follow || []) - [:_eps]
      unless terminals.empty?
        @follow ||= []
        @follow += terminals
      end
      terminals.length
    end

    private
    def ttl_expr(expr, pfx, depth, is_obj = true)
      indent = '  ' * depth
      @ebnf.debug("ttl_expr", depth: depth) {expr.inspect} if @ebnf
      op, *expr = expr if expr.is_a?(Array)
      statements = []
      
      if is_obj
        bra, ket = "[ ", " ]"
      else
        bra = ket = ''
      end

      case op
      when :seq, :alt, :diff
        # Multiple operands
        statements << %{#{indent}#{bra}#{pfx}:#{op} (}
        expr.each {|a| statements += ttl_expr(a, pfx, depth + 1)}
        statements << %{#{indent} )#{ket}}
      when :opt, :plus, :star, :not
        # Single operand
        statements << %{#{indent}#{bra}#{pfx}:#{op} }
        statements += ttl_expr(expr.first, pfx, depth + 1)
        statements << %{#{indent} #{ket}} unless ket.empty?
      when :rept
        # Three operands (min, max and expr)
        statements << %{  #{indent}#{pfx}:min #{expr[0].inspect};}
        statements << %{  #{indent}#{pfx}:max #{expr[1].inspect};}
        statements << %{#{indent}#{bra}#{pfx}:#{op} }
        statements += ttl_expr(expr.last, pfx, depth + 1)
        statements << %{#{indent} #{ket}} unless ket.empty?
      when :_empty, :_eps
        statements << %{#{indent}"g:#{op.to_s[1..-1]}"}
      when :"'"
        statements << %{#{indent}"#{esc(expr)}"}
      when :istr
        statements << %{#{indent}#{bra} re:matches #{expr.first.inspect} #{ket}}
      when :range
        statements << %{#{indent}#{bra} re:matches #{cclass(expr.first).inspect} #{ket}}
      when :hex
        raise "didn't expect \" in expr" if expr.include?(:'"')
        statements << %{#{indent}#{bra} re:matches #{cclass(expr.first).inspect} #{ket}}
      else
        if is_obj
          statements << %{#{indent}#{expr.inspect}}
        else
          statements << %{#{indent}g:seq ( #{expr.inspect} )}
        end
      end
      
      statements.last << " ." unless is_obj
      @ebnf.debug("statements", depth: depth) {statements.join("\n")} if @ebnf
      statements
    end
    
    ##
    # turn an XML BNF character class into an N3 literal for that
    # character class (less the outer quote marks)
    #
    #     >>> cclass("^<>'{}|^`")
    #     "[^<>'{}|^`]"
    #     >>> cclass("#x0300-#x036F")
    #     "[\\u0300-\\u036F]"
    #     >>> cclass("#xC0-#xD6")
    #     "[\\u00C0-\\u00D6]"
    #     >>> cclass("#x370-#x37D")
    #     "[\\u0370-\\u037D]"
    #     
    #     as in: ECHAR ::= '\' [tbnrf\"']
    #     >>> cclass("tbnrf\\\"'")
    #     'tbnrf\\\\\\"\''
    #     
    #     >>> cclass("^#x22#x5C#x0A#x0D")
    #     '^\\u0022\\\\\\u005C\\u000A\\u000D'
    def cclass(txt)
      '[' +
      txt.gsub(/\#x[0-9a-fA-F]+/) do |hx|
        hx = hx[2..-1]
        if hx.length <= 4
          "\\u#{'0' * (4 - hx.length)}#{hx}" 
        elsif hx.length <= 8
          "\\U#{'0' * (8 - hx.length)}#{hx}" 
        end
      end +
      ']'
    end

    # Make a new symbol/number combination
    # @param [String] variation added to symbol to aid reconstitution from BNF to EBNF
    def make_sym_id(variation = nil)
      @id_seq ||= 0
      @id_seq += 1
      ["_#{@sym}_#{@id_seq}#{variation}".to_sym, ("#{@id}.#{@id_seq}#{variation}" if @id)]
    end

    # Escape "[", "]", and "\" in ranges so they don't result in a warning or error
    # about empty character classes.
    def escape_regexp_character_range(character_range)
      character_range.gsub(/([\[\]\\])/) {|char| "\\#{char}"}
    end
  end
end
