# coding: utf-8
$:.unshift "."
require 'spec_helper'
require 'ebnf'
require 'sxp'
require 'nokogiri'

describe EBNF::Writer do
  RSpec::Matchers.define :have_xpath do |path, value|
    match do |actual|
      doc = Nokogiri::HTML.parse(actual)
      return false unless doc
      @result = doc.at_xpath(path.to_s) rescue false
      case value
      when false
        @result.nil?
      when true
        !@result.nil?
      when Array
        @result.to_s.split(" ").include?(*value)
      when Regexp
        @result.to_s =~ value
      else
        @result.to_s == value
      end
    end

    failure_message do |actual|
      msg = "expected that #{path.inspect}\nwould be: #{value.inspect}"
      msg += "\n     was: #{@result}"
      msg += "\nsource:" + actual
      msg
    end

    failure_message_when_negated do |actual|
      msg = "expected that #{path.inspect}\nwould not be #{value.inspect}"
      msg += "\nsource:" + actual
      msg
    end
  end

  describe ".string" do
    {
      prolog: [
        %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*},
        %{[2] Prolog ::= BaseDecl? PrefixDecl*\n}
      ],
    }.each do |title, (grammar, plain)|
      context title do
        subject {EBNF::Base.new(grammar, format: :native).ast}

        it "generates plain" do
          expect(EBNF::Writer.string(*subject)).to eq plain
        end
      end
    end
  end

  describe ".print" do
    {
      prolog: [
        %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*},
        %{[2] Prolog ::= BaseDecl? PrefixDecl*\n}
      ],
    }.each do |title, (grammar, plain)|
      context title do
        subject {EBNF::Base.new(grammar, format: :native).ast}

        it "generates plain" do
          expect {EBNF::Writer.print(*subject)}.to write(plain).to(:output)
        end
      end
    end
  end

  describe ".html" do
    {
      prolog: [
        %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*},
        {
          '//table/@class': "grammar",
          '//table/tbody/@id': "grammar-productions",
          '//tbody/tr/@id': "grammar-production-Prolog",
          '//tbody/tr/td[1]/text()': "[2]",
          '//tbody/tr/td[2]/code/text()': "Prolog",
          '//tbody/tr/td[3]/text()': "::=",
          #'//tbody/tr/td[4]/*/text()': /BaseDecl\? PrefixDecl\*/,
        }
      ],
    }.each do |title, (grammar, xpaths)|
      context title do
        subject {EBNF::Writer.html(*EBNF::Base.new(grammar, format: :native).ast)}

        specify {is_expected.to be_valid_html}

        xpaths.each do |path, value|
          specify {is_expected.to have_xpath(path, value)}
        end
      end
    end
  end

  context "EBNF" do
    describe "#initialize" do
      {
        prolog: [
          %{[2]     Prolog    ::=           BaseDecl? PrefixDecl*},
          %{[2] Prolog ::= BaseDecl? PrefixDecl*\n}
        ],
      }.each do |title, (grammar, plain)|
        context title do
          subject {EBNF::Base.new(grammar, format: :native).ast}

          it "generates plain" do
            expect {EBNF::Writer.new(subject)}.to write(plain).to(:output)
          end
        end
      end
    end

    describe "#format_ebnf" do
      subject {EBNF::Writer.new([])}

      context "legal expressions" do
        {
          "alt": [
            [:alt, :A, :B],
            "A | B"
          ],
          "diff": [
            [:diff, :A, :B],
            "A - B"
          ],
          "hex": [
            [:hex, "#x20"],
            "#x20"
          ],
          "istr": [
            [:istr, "foo"],
            %("foo")
          ],
          "opt": [
            [:opt, :A],
            "A?"
          ],
          "plus": [
            [:plus, :A],
            "A+"
          ],
          "range": [
            [:range, "a-zA-Z"],
            "[a-zA-Z]"
          ],
          "rept 0 1": [
            [:rept, 0, 1, :A],
            "A?"
          ],
          "rept 0 *": [
            [:rept, 0, '*', :A],
            "A*"
          ],
          "rept 1 1": [
            [:rept, 1, 1, :A],
            "A"
          ],
          "rept 1 *": [
            [:rept, 1, '*', :A],
            "A+"
          ],
          "rept 1 2": [
            [:rept, 1, 2, :A],
            "A A?"
          ],
          "rept 1 3": [
            [:rept, 1, 3, :A],
            "A (A A?)?"
          ],
          "rept 2 *": [
            [:rept, 2, "*", :A],
            "A A A*"
          ],
          "rept 1 3 (A B)": [
            [:rept, 1, 3, [:seq, :A, :B]],
            "(A B) ((A B) (A B)?)?"
          ],
          "rept 1 3 (A | B)": [
            [:rept, 1, 3, [:alt, :A, :B]],
            "(A | B) ((A | B) (A | B)?)?"
          ],
          "star": [
            [:star, :A],
            "A*"
          ],
          "string '\\r'": [
            [:seq, "\r"],
            %{#x0D}
          ],
          "string ' '": [
            [:seq, " "],
            %{#x20}
          ],
          "string 'a'": [
            [:seq, "a"],
            %{"a"}
          ],
          "string '\"'": [
            [:seq, '"'],
            %{'"'}
          ],
          "string \"'\"": [
            [:seq, '\''],
            %{"'"}
          ],
          "string \"\€\"": [
            [:seq, '€'],
            %{"€"}
          ],
          "n3 path": [
            [:seq, :pathItem, [:alt, [:seq, "!", :path], [:seq, "^", :path]]],
            %{pathItem (("!" path) | ("^" path))}
          ],
        }.each do |title, (expr, result)|
          it title do
            expect(subject.send(:format_ebnf, expr)).to eql result
          end
        end
      end

      context "illegal expressions" do
        {
          "string 'a\nb": [:seq, "a\nb"],
        }.each do |title, expr|
          it title do
            expect {subject.send(:format_ebnf, expr)}.to raise_error RangeError
          end
        end
      end
    end

    context "Existing grammars" do
      {
        "ABNF Grammar" => File.expand_path("../../etc/abnf.ebnf", __FILE__),
        "EBNF Grammar" => File.expand_path("../../etc/ebnf.ebnf", __FILE__),
        "ISO EBNF Grammar" => File.expand_path("../../etc/iso-ebnf.ebnf", __FILE__),
        "Turtle Grammar" => File.expand_path("../../etc/turtle.ebnf", __FILE__),
        "SPARQL Grammar" => File.expand_path("../../etc/sparql.ebnf", __FILE__),
      }.each do |name, file|
        context name do
          it "outputs grammar as text" do
            expect {EBNF.parse(File.read(file)).to_s}.to_not raise_error
          end
          it "parses to equivalent rules" do
            expect(EBNF.parse(File.read(file)).to_sxp).to produce(File.read(file.sub('.ebnf', '.sxp')))
          end
          it "outputs grammar as html" do
            html = nil
            expect {html = EBNF.parse(File.read(file)).to_html}.to_not raise_error
            expect(html).to be_valid_html
          end
        end
      end
    end
  end

  context "ABNF" do
    describe "#initialize" do
      {
        prolog: [
          %{rulelist       =  1*( rule / (*c-wsp c-nl) )\n},
          %{rulelist = 1*(rule / (*c-wsp c-nl))\n}
        ],
      }.each do |title, (grammar, plain)|
        context title do
          subject {EBNF::Base.new(grammar, format: :abnf).ast}

          it "generates plain" do
            expect {EBNF::Writer.new(subject, format: :abnf)}.to write(plain).to(:output)
          end
        end
      end
    end

    describe "#format_abnf" do
      subject {EBNF::Writer.new([])}

      context "legal expressions" do
        {
          "alt": [
            [:alt, :A, :B],
            "A / B"
          ],
           "enum": [
            [:range, "abc-"],
            "%d97.98.99.45"
          ],
          "hex": [
            [:hex, "#x20"],
            "%x20"
          ],
          "istr": [
            [:istr, "foo"],
            %("foo")
          ],
          "opt": [
            [:opt, :A],
            "[A]"
          ],
          "plus": [
            [:plus, :A],
            "1*A"
          ],
          "range": [
            [:range, "a-z"],
            "%d97-122"
          ],
          "range 2": [
            [:range, "a-zA-Z"],
            %{(%d97-122 / %d65-90)}
          ],
          "rept 0 1": [
            [:rept, 0, 1, :A],
            "0*1A"
          ],
          "rept 0 *": [
            [:rept, 0, '*', :A],
            "*A"
          ],
          "rept 1 1": [
            [:rept, 1, 1, :A],
            "1A"
          ],
          "rept 1 *": [
            [:rept, 1, '*', :A],
            "1*A"
          ],
          "rept 1 2": [
            [:rept, 1, 2, :A],
            "1*2A"
          ],
          "rept 1 3": [
            [:rept, 1, 3, :A],
            "1*3A"
          ],
          "rept 2 *": [
            [:rept, 2, "*", :A],
            "2*A"
          ],
          "rept 1 3 (A B)": [
            [:rept, 1, 3, [:seq, :A, :B]],
            "1*3(A B)"
          ],
          "rept 1 3 (A | B)": [
            [:rept, 1, 3, [:alt, :A, :B]],
            "1*3(A / B)"
          ],
          "star": [
            [:star, :A],
            "*A"
          ],
          "string '\\r'": [
            [:seq, "\r"],
            %{%x0D}
          ],
          "string ' '": [
            [:seq, " "],
            %{" "}
          ],
          "string 'a'": [
            [:seq, "a"],
            %{"a"}
          ],
          "string '\"'": [
            [:seq, '"'],
            %{%x22}
          ],
          "string \"'\"": [
            [:seq, '\''],
            %{"'"}
          ],
          "string \"\€\"": [
            [:seq, '€'],
            %{%x20AC}
          ],
          "n3 path": [
            [:seq, :pathItem, [:alt, [:seq, "!", :path], [:seq, "^", :path]]],
            %{pathItem (("!" path) / ("^" path))}
          ],
        }.each do |title, (expr, result)|
          it title do
            expect(subject.send(:format_abnf, expr)).to eql result
          end
        end
      end

      context "illegal expressions" do
        {
          "[^abc]": [:range, "^abc"],
          "A - B":  [:diff, :A, :B],
        }.each do |title, expr|
          it title do
            expect {subject.send(:format_abnf, expr)}.to raise_error RangeError
          end
        end
      end
    end

    context "Existing grammars" do
      {
        "ABNF Grammar" => File.expand_path("../../etc/abnf.abnf", __FILE__),
        "HTTP Grammar" => File.expand_path("../../examples/abnf/examples/http.abnf", __FILE__),
        "JSON Grammar" => File.expand_path("../../examples/abnf/examples/json.abnf", __FILE__),
        "Postal Address" => File.expand_path("../../examples/abnf/examples/postal-address.abnf", __FILE__),
        "URI Grammar" => File.expand_path("../../examples/abnf/examples/uri.abnf", __FILE__),
      }.each do |name, file|
        context name do
          it "outputs grammar as text" do
            expect {EBNF.parse(File.read(file), format: :abnf).to_s(format: :abnf)}.to_not raise_error
          end
          it "outputs grammar as html" do
            html = nil
            expect {html = EBNF.parse(File.read(file), format: :abnf).to_html(format: :abnf)}.to_not raise_error
            expect(html).to be_valid_html
          end
        end
      end
    end
  end

  context "ISOEBNF" do
    describe "#initialize" do
      {
        prolog: [
          %{syntax            = syntax_rule, {syntax_rule} ;},
          %{syntax = syntax_rule, {syntax_rule} ;\n}
        ],
      }.each do |title, (grammar, plain)|
        context title do
          subject {EBNF::Base.new(grammar, format: :isoebnf).ast}

          it "generates plain" do
            expect {EBNF::Writer.new(subject, format: :isoebnf)}.to write(plain).to(:output)
          end
        end
      end
    end

    describe "#format_isoebnf" do
      subject {EBNF::Writer.new([])}

      context "legal expressions" do
        {
          "alt": [
            [:alt, :A, :B],
            "A | B"
          ],
          "diff": [
            [:diff, :A, :B],
            "A - B"
          ],
           "enum": [
            [:range, "abc-"],
            %{("a" | "b" | "c" | "-")}
          ],
          "hex": [
            [:hex, "#x20"],
            %(" ")
          ],
          "istr": [
            [:istr, "foo"],
            %("foo")
          ],
          "opt": [
            [:opt, :A],
            "[A]"
          ],
          "plus": [
            [:plus, :A],
            "A, {A}"
          ],
          "range": [
            [:range, "a-z"],
            %{("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z")}
          ],
          "range 2": [
            [:range, "a-zA-Z"],
            %{("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z")}],
          "rept 0 1": [
            [:rept, 0, 1, :A],
            "[A]"
          ],
          "rept 0 *": [
            [:rept, 0, '*', :A],
            "{A}"
          ],
          "rept 1 1": [
            [:rept, 1, 1, :A],
            "A"
          ],
          "rept 1 *": [
            [:rept, 1, '*', :A],
            "A, {A}"
          ],
          "rept 1 2": [
            [:rept, 1, 2, :A],
            "A, [A]"
          ],
          "rept 1 3": [
            [:rept, 1, 3, :A],
            "A, [(A, [A])]"
          ],
          "rept 2 *": [
            [:rept, 2, "*", :A],
            "A, A, {A}"
          ],
          "rept 1 3 (A B)": [
            [:rept, 1, 3, [:seq, :A, :B]],
            "(A, B), [((A, B), [(A, B)])]"
          ],
          "rept 1 3 (A | B)": [
            [:rept, 1, 3, [:alt, :A, :B]],
            "(A | B), [((A | B), [(A | B)])]"
          ],
          "star": [
            [:star, :A],
            "{A}"
          ],
          "string ' '": [
            [:seq, " "],
            %{" "}
          ],
          "string 'a'": [
            [:seq, "a"],
            %{"a"}
          ],
          "string '\"'": [
            [:seq, '"'],
            %{'"'}
          ],
          "string \"'\"": [
            [:seq, '\''],
            %{"'"}
          ],
          "n3 path": [
            [:seq, :pathItem, [:alt, [:seq, "!", :path], [:seq, "^", :path]]],
            %{pathItem, (("!", path) | ("^", path))}
          ],
        }.each do |title, (expr, result)|
          it title do
            expect(subject.send(:format_isoebnf, expr)).to eql result
          end
        end
      end

      context "illegal expressions" do
        {
          "[^abc]": [:range, "^abc"],
          "string '\\r'": [:seq, "\r"],
          "string \"\€\"": [:seq, '€'],
        }.each do |title, expr|
          it title do
            expect {subject.send(:format_isoebnf, expr)}.to raise_error RangeError
          end
        end
      end
    end

    context "Existing grammars" do
      {
        "ISO EBNF Grammar" => File.expand_path("../../etc/iso-ebnf.isoebnf", __FILE__),
        "Simiple EBNF Grammar" => File.expand_path("../../examples/isoebnf/examples/ebnf.isoebnf", __FILE__),
        "HTML Grammar" => File.expand_path("../../examples/isoebnf/examples/html.isoebnf", __FILE__),
        "Pascal Grammar" => File.expand_path("../../examples/isoebnf/examples/pascal.isoebnf", __FILE__),
        "Postal Address" => File.expand_path("../../examples/isoebnf/examples/postal-address.isoebnf", __FILE__),
      }.each do |name, file|
        context name do
          it "outputs grammar as text" do
            expect {EBNF.parse(File.read(file), format: :isoebnf).to_s(format: :isoebnf)}.to_not raise_error
          end
          it "outputs grammar as html" do
            html = nil
            expect {html = EBNF.parse(File.read(file), format: :isoebnf).to_html(format: :isoebnf)}.to_not raise_error
            expect(html).to be_valid_html
          end
        end
      end
    end
  end
end
