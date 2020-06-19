module EBNF
  ##
  # This module extends {EBNF::Base} to create metadata including _branch_,  [First/Follow][], and other tables which is used by {EBNF::LL1::Parser} to recognize examples of the associated grammar.
  #
  ### Branch Table
  #
  #  The Branch table is a hash mapping production rules to a hash relating terminals appearing in input to sequence of productions to follow when the corresponding input terminal is found. This allows either the `seq` primitive, where all terminals map to the same sequence of productions, or the `alt` primitive, where each terminal may map to a different production.
  #
  #      BRANCH = {
  #        :alt => {
  #          "(" => [:seq, :_alt_1],
  #          :ENUM => [:seq, :_alt_1],
  #          :HEX => [:seq, :_alt_1],
  #          :O_ENUM => [:seq, :_alt_1],
  #          :O_RANGE => [:seq, :_alt_1],
  #          :RANGE => [:seq, :_alt_1],
  #          :STRING1 => [:seq, :_alt_1],
  #          :STRING2 => [:seq, :_alt_1],
  #          :SYMBOL => [:seq, :_alt_1],
  #        },
  #        ...
  #        :declaration => {
  #          "@pass" => [:pass],
  #          "@terminals" => ["@terminals"],
  #        },
  #        ...
  #      }
  #
  #  In this case the `alt` rule is `seq ('|' seq)*` can happen when any of the specified tokens appears on the input stream. The all cause the same token to be passed to the `seq` rule and follow with `_alt_1`, which handles the `('|' seq)*` portion of the rule, after the first sequence is matched.
  #
  #  The `declaration` rule is `@terminals' | pass` using the `alt` primitive determining the production to run based on the terminal appearing on the input stream. Eventually, a terminal production is found and the token is consumed.
  #
  ### First/Follow Table
  #
  #  The [First/Follow][] table is a hash mapping production rules to the terminals that may proceed or follow the rule. For example:
  #
  #      FIRST = {
  #        :alt => [
  #          :HEX,
  #          :SYMBOL,
  #          :ENUM,
  #          :O_ENUM,
  #          :RANGE,
  #          :O_RANGE,
  #          :STRING1,
  #          :STRING2,
  #          "("],
  #        ...
  #      }
  #
  ### Terminals Table
  #
  #  This table is a simple list of the terminal productions found in the grammar. For example:
  #
  #      TERMINALS = ["(", ")", "-",
  #        "@pass", "@terminals",
  #        :ENUM, :HEX, :LHS, :O_ENUM, :O_RANGE,:POSTFIX,
  #        :RANGE, :STRING1, :STRING2, :SYMBOL,"|"
  #      ].freeze
  #
  ### Cleanup Table
  #
  #  This table identifies productions which used EBNF rules, which are transformed to BNF for actual parsing. This allows the parser, in some cases, to reproduce *star*, *plus*, and *opt* rule matches. For example:
  #
  #      CLEANUP = {
  #        :_alt_1 => :star,
  #        :_alt_3 => :merge,
  #        :_diff_1 => :opt,
  #        :ebnf => :star,
  #        :_ebnf_2 => :merge,
  #        :_postfix_1 => :opt,
  #        :seq => :plus,
  #        :_seq_1 => :star,
  #        :_seq_2 => :merge,
  #      }.freeze
  #
  #  In this case the `ebnf` rule was `(declaration | rule)*`. As BNF does not support a star operator, this is decomposed into a set of rules using `alt` and `seq` primitives:
  #
  #      ebnf    ::= _empty _ebnf_2
  #      _ebnf_1 ::= declaration | rule
  #      _ebnf_2 ::= _ebnf_1 ebnf
  #      _ebnf_3 ::= ebnf
  #
  #  The `_empty` production matches an empty string, so allows for now value. `_ebnf_2` matches `declaration | rule` (using the `alt` primitive) followed by `ebnf`, creating a sequence of zero or more `declaration` or `alt` members.
  #
  # [First/Follow]: https://en.wikipedia.org/wiki/LL_parser#Constructing_an_LL.281.29_parsing_table

  module LL1
    autoload :Lexer,    "ebnf/ll1/lexer"
    autoload :Parser,   "ebnf/ll1/parser"
    autoload :Scanner,  "ebnf/ll1/scanner"

    # Branch table, represented as a recursive hash.
    # The table is indexed by rule symbol, which in-turn references a hash of terminals (which are the first terminals of the production), which in turn reference the sequence of rules that follow, given that terminal as input
    #
    # @return [Hash{Symbol => Hash{String, Symbol => Array<Symbol>}}]
    attr_reader :branch

    # First table
    #
    # @return [Hash{Symbol => Array<String, Symbol>}]
    attr_reader :first

    # Follow table
    #
    # @return [Hash{Symbol => Array<String, Symbol>}]
    attr_reader :follow

    # EBNF Cleanup table
    #
    # The list of terminals used in the grammar.
    #
    # @return [Hash{Symbol => Symbol}]
    attr_reader :cleanup

    # Terminal table
    #
    # The list of terminals used in the grammar.
    #
    # @return [Array<String, Symbol>]
    attr_reader :terminals

    # Pass expression
    #
    # A Terminal symbol used for skipping whitespace and comments
    #
    # @return [Symbol, String]
    attr_reader :pass

    # Start symbol
    #
    # The rule which starts the grammar
    #
    # @return [Symbol]
    attr_reader :start

    ##
    # Create first/follow for each rule using techniques defined for LL(1) parsers.
    #
    # This takes rules which have transformed into BNF and adds first/follow and otehr information to the rules to allow the generation of metadata tables used for driving a parser.
    #
    # Given an initial rule in EBNF:
    #
    #     (rule enbf "1" (star declaration rule))
    #
    # The BNF transformation becomes:
    #
    #     (rule ebnf "1" (alt _empty _ebnf_2))
    #     (rule _ebnf_1 "1.1" (alt declaration rule))
    #     (rule _ebnf_2 "1.2" (seq _ebnf_1 ebnf))
    #     (rule _ebnf_3 "1.3" (seq ebnf))
    #
    # After running this method, the rules are annotated with first/follow and cleanup rules:
    #
    #     (rule ebnf "1"
    #      (start #t)
    #      (first "@pass" "@terminals" LHS _eps)
    #      (follow _eof)
    #      (cleanup star)
    #      (alt _empty _ebnf_2))
    #     (rule _ebnf_1 "1.1"
    #      (first "@pass" "@terminals" LHS)
    #      (follow "@pass" "@terminals" LHS _eof)
    #      (alt declaration rule))
    #     (rule _ebnf_2 "1.2"
    #      (first "@pass" "@terminals" LHS)
    #      (follow _eof)
    #      (cleanup merge)
    #      (seq _ebnf_1 ebnf))
    #     (rule _ebnf_3 "1.3" (first "@pass" "@terminals" LHS _eps) (follow _eof) (seq ebnf))
    #
    # @return [EBNF] self
    # @see https://en.wikipedia.org/wiki/LL_parser#Constructing_an_LL.281.29_parsing_table
    # @param [Array<Symbol>] starts
    #   Set of symbols which are start rules
    def first_follow(*starts)
      # Add _eof to follow all start rules
      @starts = starts
      if @start = starts.first
        starts.each do |start|
          start_rule = find_rule(start)
          raise "No rule found for start symbol #{start}" unless start_rule
          start_rule.add_follow([:_eof])
          start_rule.start = true
        end
      end

      # Comprehnsion rule, create shorter versions of all non-terminal sequences. This is necessary as the FF rules reference w', which is a comprehension.
      comprehensions = []
      ittr = 0
      depth do
        begin
          comprehensions = []
          ast.select {|r| r.rule? && r.seq? && r.comp.nil? && r.expr.length > 2}.each do |rule|
            new_expr = rule.expr[2..-1].unshift(:seq)
            if new_rule = ast.detect {|r| r.expr == new_expr}
              # Link to existing comprehension used for another rules
              debug("FF.c") {"(#{ittr}) link comprehension rule for #{rule.sym} => #{new_rule.sym}[#{new_expr.inspect}]"}
            else
              new_rule = rule.build(new_expr)
              debug("FF.c") {"(#{ittr}) add comprehension rule for #{rule.sym} => #{new_rule.sym}[#{new_expr.inspect}]"}
              comprehensions << new_rule
            end
            rule.comp = new_rule
          end

          @ast += comprehensions
          progress("FF.c") {"(#{ittr}) comprehensions #{comprehensions.length}"}
          ittr += 1
        end while !comprehensions.empty?

        ittr = 0
        begin
          firsts, follows = 0, 0
          # add Fi(wi) to Fi(Ai) for every rule Ai → wi
          #
          # For sequences, this is the first rule in the sequence.
          # For alts, this is every rule in the sequence
          each(:rule) do |ai|
            # Fi(a w' ) = { a } for every terminal a
            ai.terminals(ast).each do |t|
              debug("Fi.2.1") {"(#{ittr}) add terminal #{t} to #{ai.sym}"}
              firsts += ai.add_first([t])
            end

            ai.non_terminals(ast).select(&:first).each do |a|
              if !a.first_includes_eps?
                # Fi(A w' ) = Fi(A) for every nonterminal A with ε not in Fi(A)
                debug("Fi.2.2") {"(#{ittr}) add first from #{a.sym} to #{ai.sym}: #{a.first.inspect}"}
                firsts += ai.add_first(a.first)
              else
                # Fi(A w' ) = Fi(A) \ { ε } ∪ Fi(w' ) for every nonterminal A with ε in Fi(A)
                if ai.seq?
                  # w' is either comprehnsion of ai, or empty, if there is no comprehension
                  comp = ai.comp || find_rule(:_empty)

                  fi = a.first - [:_eps] + (comp.first || [])
                  debug("Fi.2.3a") {"(#{ittr}) add first #{fi.inspect} from #{a.sym} and #{comp.sym} to #{ai.sym}"}
                  firsts += ai.add_first(fi)
                else
                  # ai is an alt, so there are no comprehensions of non-terminals, add Fi(A) including ε
                  debug("Fi.2.3b") {"(#{ittr}) add first #{a.first} from #{a.sym} to #{ai.sym}"}
                  firsts += ai.add_first(a.first)
                end
              end
            end
          end

          # # Fi(ε) = { ε }
          # Add _eps as a first of _empty
          find_rule(:_empty).add_first([:_eps])

          # Add follows
          # if there is a rule of the form Aj → wAiw' , then
          # First do this for the case when Ai is the first rule
          each(:rule) do |aj|
            comp = aj.comp || find_rule(:_empty)
            aj.non_terminals(ast).reject {|r| r.sym == :_empty}.each do |ai|
              # if the terminal a is in Fi(w' ), then add a to Fo(Ai)
              # Basically, this says that the firsts of a comprehension of a rule are the follows of the first non-terminal in the rule.
              if comp.first
                debug("Fo.2.1") {"(#{ittr}) add follow #{comp.first.inspect} from #{comp.sym} to #{ai.sym}"}
                follows += ai.add_follow(comp.first)
              end

              # If there is no comprehension of this rule (meaning, it is a sequence of one non-terminal), then the follows of the non-terminal include the follows of the rule. This handles rules with multiple sequences because it will have a comprehension that includes the last element in the sequence
              if !aj.comp && aj.follow
                debug("Fo.2.1a") {"(#{ittr}) add follow #{aj.follow.inspect} from #{aj.sym} to #{ai.sym}"}
                follows += ai.add_follow(aj.follow)
              end

              # if ε is in Fi(w' ), then add Fo(Aj) to Fo(Ai)
              if comp.first_includes_eps? && aj.follow
                debug("Fo.2.2") {"(#{ittr}) add follow #{aj.follow.inspect} from #{aj.sym} to #{ai.sym}"}
                follows += ai.add_follow(aj.follow)
              end
            end

            # Since the rules are of the form wAiw', and we've handled the case which is just Aiw', this leaves those cases that have rules prior to Ai. This basically says that the follows of a rule are added to the follows of the comprehension of the rule
            if aj.comp && aj.follow
              debug("Fo.2.3") {"(#{ittr}) add follow #{aj.follow.inspect} from #{aj.sym} to #{aj.comp.sym}"}
              follows += aj.comp.add_follow(aj.follow)
            end
          end

          progress("first_follow") {"(#{ittr}) firsts #{firsts}, follows #{follows}"}
          ittr += 1
        end while (firsts + follows) > 0

        debug("Fi.2-post: non-terminals without first") do
          ast.reject(&:terminal?).reject(&:first).map(&:sym)
        end if ast.reject(&:terminal?).any? {|r| r.first.nil?}
      end
    end

    ##
    # Generate parser tables, {#branch}, {#first}, {#follow}, and {#terminals}
    def build_tables
      progress("build_tables") {
        "Terminals: #{ast.count {|r| r.terminal?}} " +
        "Non-Terminals: #{ast.count {|r| r.rule?}}"
      }

      @first = ast.
        select(&:first).
        inject({}) {|memo, r|
          memo.merge(r.sym => r.first)
        }
      @follow = ast.
        select(&:follow).
        inject({}) {|memo, r|
          memo.merge(r.sym => r.follow)
        }

      @cleanup = ast.
        select(&:cleanup).
        inject({}) {|memo, r| memo.merge(r.sym => r.cleanup)}

      @terminals = ast.map {|r| Array(r.first) + Array(r.follow)}.flatten.uniq
      @terminals = (@terminals - [:_eps, :_eof]).sort_by{|t| t.to_s.sub(/^_/, '')}
      # FIXME: assumes that this is a (seq :PASS), or similar
      if pass = ast.detect {|r| r.pass?}
        @pass = pass.expr.last
      end

      # If a generated terminal is found, this indicates an error, as this version does not automatically generate regular expressions for automatic terminals
      @terminals.
        select {|t| t.to_s.start_with?("_")}.
        reject {|t| t.to_s.start_with?("_pass_")}.  # Concession to backwards compatibility
        each do |term|

        error("build_tables",
              "terminal #{term} is automatically generated; " +
              "regular expressions are not yet generated and parsing " +
              "is not supported")
      end

      @branch = {}
      @already = []
      @agenda = []
      Array(@starts).each do |start|
        do_production(start)
        while !@agenda.empty?
          x = @agenda.shift
          do_production(x)
        end
      end

      if !@errors.empty?
        progress("###### FAILED with #{errors.length} errors.")
        @errors.each {|s| progress("  #{s}")}
        raise "Table creation failed with errors"
      else
        progress("Ok for predictive parsing")
      end 
    end

    # Generate an output table in Ruby format
    # @param [IO, StringIO] io
    # @param [String] name of the table constant
    # @param [String] table
    #   to output, one of {#branch}, {#first}, {#follow}, {#cleanup} or {#terminals}
    # @param [Integer] indent = 0
    def outputTable(io, name, table, indent = 0)
      ind0 = '  ' * indent
      ind1 = ind0 + '  '
      ind2 = ind1 + '  '

      if table.is_a?(Hash)
        io.puts "#{ind0}#{name} = {"
        table.keys.sort_by{|t| t.to_s.sub(/^_/, '')}.each do |prod|
          case table[prod]
          when Symbol, String
            io.puts "#{ind1}#{prod.inspect} => #{table[prod].inspect},"
          when Array
            list = table[prod].map(&:inspect).join(",\n#{ind2}")
            io.puts "#{ind1}#{prod.inspect} => [\n#{ind2}#{list}],"
          when Hash
            io.puts "#{ind1}#{prod.inspect} => {"
            table[prod].keys.sort_by{|t| t.to_s.sub(/^_/, '')}.each do |term|
              list = table[prod][term].map(&:inspect).join(", ")
              io.puts "#{ind2}#{term.inspect} => [#{list}],"
            end
            io.puts "#{ind1}},"
          else
            "Unknown table entry type: #{table[prod].class}"
          end
        end
        io.puts "#{ind0}}.freeze\n"
      elsif table
        io.puts "#{ind0}#{name} = [\n#{ind1}" +
          table.sort_by{|t| t.to_s.sub(/^_/, '')}.map(&:inspect).join(",\n#{ind1}") +
          "\n#{ind0}].freeze\n"
      end
    end

    ##
    # Output Ruby parser files for LL(1) parsing
    #
    # @param [IO, StringIO] output
    def to_ruby_ll1(output, **options)
      self.outputTable(output, 'BRANCH', self.branch, 1)
      self.outputTable(output, 'TERMINALS', self.terminals, 1)
      self.outputTable(output, 'FIRST', self.first, 1)
      self.outputTable(output, 'FOLLOW', self.follow, 1)
      self.outputTable(output, 'CLEANUP', self.cleanup, 1)
      self.outputTable(output, 'PASS', [self.pass], 1) if self.pass
    end

    private
    def do_production(lhs)
      rule = find_rule(lhs)
      if rule.nil? || !rule.rule? || rule.sym == :_empty
        progress("prod") {"Skip: #{lhs.inspect}"}
        return
      end
      @already << lhs

      branchDict = {}

      progress("prod") {"Production #{lhs.inspect}"}

      if rule.expr.first == :matches
        debug("prod") {"Rule is regexp: #{rule}"}
        return
      end

      error("No record of what token #{lhs.inspect} can start with") unless rule.first

      if rule.alt?
        # A First/Follow conflict appears when _eps is in the first
        # of one rule and there is a token in the first and
        # follow of the same rule
        if Array(rule.first).include?(:_eps) && !(overlap = ((Array(rule.first) & (rule.follow || [])) - [:eps])).empty?
          error("First/Follow Conflict: #{overlap.first.inspect} is both first and follow of #{rule.sym}")
        end

        # Add entries for each alternative, based on the alternative's first/seq
        rule.expr[1..-1].each do |prod|
          prod_rule = find_rule(prod)
          debug("  Alt", prod)

          @agenda << prod unless @already.include?(prod) || @agenda.include?(prod)
          if prod == :_empty
            debug("    empty")
            # Skip empty, rules added bellow for follows
          elsif prod_rule.nil? || prod_rule.first.nil?
            debug("    no first =>", prod)
            branchDict[prod] = [prod]
          else
            prod_rule.first.reject{|f| f == :_eps}.each do |f|
              # A First/First conflict appears when there are two rules having
              # the same first, so the parser can't know which one to choose.
              if branchDict.has_key?(f)
                error("First/First Conflict: #{f.inspect} is the condition for both #{prod_rule.sym} and #{branchDict[f].first}")
              end

              debug("   alt") {"[#{f}] => #{prod}"}
              branchDict[f] = [prod]
            end
          end
        end
      else
        error("prod") {"Expected lhs to be alt or seq, was: #{rule}"} unless rule.seq?
        debug("  Seq", rule)
        # Entries for each first element referencing the sequence
        (rule.first || []).each do |f|
          if [:_eps, :_eof].include?(f)
            # Skip eps/eof, rules added below for follows
          else
            debug("   seq") {"[#{f}] => #{rule.expr[1..-1].inspect}"}
            branchDict[f] = rule.expr[1..-1]
          end
        end
      
        # Add each production to the agenda
        rule.expr[1..-1].each do |prod|
          @agenda << prod unless @already.include?(prod) || @agenda.include?(prod)
        end
      end

      # Add follow rules, if first includes eps
      if rule.first_includes_eps?
        (rule.follow || []).reject {|f| f == :_eof}.each do |f|
          debug("  Follow") {f.inspect}
          branchDict[f] ||= []
        end
      end

      @branch[lhs] = branchDict
    end
  end
end
