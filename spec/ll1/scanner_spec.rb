# coding: utf-8
$:.unshift ".."
require 'spec_helper'
require 'ebnf'

describe EBNF::LL1::Scanner do
  describe ".new" do
    it "initializes with a StringIO" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("line1\nline2\n"))
      expect(scanner.rest).to eq "line1\nline2\n"
      expect(scanner).not_to be_eos
    end

    it "initializes with a string" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new "line1\nline2\n")
      expect(scanner.rest).to eq "line1\nline2\n"
      expect(scanner).not_to be_eos
    end

    it "encodes input to UTF-8", pending: !"".respond_to?(:force_encoding) do
      f = double("input")
      expect(f).to receive(:read).and_return("ascii".force_encoding(Encoding::ASCII_8BIT))
      expect(f).to receive(:gets).and_return("utf8".force_encoding(Encoding::UTF_8))
      expect(f).to receive(:eof?).and_return(false, false, true)
      scanner = EBNF::LL1::Scanner.new(f)
      s = scanner.rest
      expect(s).to eq "asciiutf8"
      expect(s.encoding).to eq Encoding::UTF_8
    end
  end

  describe "#eos?" do
    it "returns true if at both eos and eof" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new(""))
      expect(scanner).to be_eos
    end
  end

  describe "#lineno" do
    context "STRING_LITERAL_LONG_QUOTE" do
      subject {
        EBNF::LL1::Scanner.new( %(
        :Test a rdfs:Class ;
          rdfs:subClassOf mf:ManifestEntry;
          rdfs:label "Superclass of all CSVW tests" ;
          rdfs:comment """
            All CSVW tests have an input file referenced using `mf:action`. Positive
            and Negative Evaluation Tests also have a result file referenced using
            `mf:result` . Other tests may take different inputs and options as defined
            for each test class.
          """ ;
          :b :c .
        ))
      }
      it "tracks line numbers" do
        subject.scan_until(/:Test/)
        expect(subject.lineno).to eq 2

        subject.scan_until(/rdfs:subClassOf/)
        expect(subject.lineno).to eq 3

        subject.scan(/\w+/)
        expect(subject.lineno).to eq 3

        subject.skip_until(/"""/)
        expect(subject.lineno).to eq 5

        subject.skip_until(/"""/)
        expect(subject.lineno).to eq 10
      end
    end
  end

  describe "#rest" do
    it "returns remaining scanner contents if not at eos" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("foo\n"))
      expect(scanner.rest).to eq "foo\n"
      expect(scanner.lineno).to eq 1
    end

    it "returns next line from file if at eos" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("\nfoo\n"))
      expect(scanner.rest).to eq "\nfoo\n"
      scanner.scan(/\s*/m)
      expect(scanner.rest).to eq "foo\n"
      expect(scanner.lineno).to eq 2
    end

    it "returns \"\" if at eos and eof" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new(""))
      expect(scanner.rest).to eq ""
    end
  end

  describe "#scan" do
    context "simple terminals" do
      it "returns a word" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("foo bar"))
        expect(scanner.scan(/\w+/)).to eq "foo"
        expect(scanner.lineno).to eq 1
      end

      it "returns a STRING_LITERAL_QUOTE" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("'string' foo"))
        expect(scanner.scan(/'((?:[^\x27\x5C\x0A\x0D])*)'/)).to eq "'string'"
        expect(scanner.lineno).to eq 1
      end

      it "returns a STRING_LITERAL_LONG_SINGLE_QUOTE" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("'''\nstring\nstring''' foo"))
        expect(scanner.scan(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m)).to eq "'''\nstring\nstring'''"
        expect(scanner.lineno).to eq 3
      end

      it "scans a multi-line string" do
         string = %q('''
          <html:a="b"/>
          '''
        )
        scanner = EBNF::LL1::Scanner.new(StringIO.new(string))
        expect(scanner.scan(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m)).not_to be_empty
        expect(scanner.lineno).to eq 3
      end

      it "scans a longer multi-line string" do
         string = %q('''
          <html:b xmlns:html="http://www.w3.org/1999/xhtml" html:a="b"/>
          '''
        )
        scanner = EBNF::LL1::Scanner.new(StringIO.new(string))
        expect(scanner.scan(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m)).not_to be_empty
        expect(scanner.lineno).to eq 3
      end
    end
  end

  describe "#scan_until" do
    context "simple terminals" do
      it "returns a word" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("foo bar"))
        expect(scanner.scan_until(/\w+/)).to eq "foo"
        expect(scanner.lineno).to eq 1
      end

      it "returns a STRING_LITERAL_QUOTE" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("prefix 'string' foo"))
        expect(scanner.scan_until(/'((?:[^\x27\x5C\x0A\x0D])*)'/)).to eq "prefix 'string'"
        expect(scanner.lineno).to eq 1
      end

      it "returns a STRING_LITERAL_LONG_SINGLE_QUOTE" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("prefix '''\nstring\nstring''' foo"))
        expect(scanner.scan_until(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m)).to eq "prefix '''\nstring\nstring'''"
        expect(scanner.lineno).to eq 3
      end
    end
  end

  describe "#skip" do
    it "skips input" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("foo\n"))
      scanner.skip(/^f/)
      expect(scanner.rest).to eq "oo\n"
      expect(scanner.lineno).to eq 1
    end
  end

  describe "#skip_until" do
    it "skips input" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("prefix\nfoo\n"))
      scanner.skip_until(/^f/)
      expect(scanner.rest).to eq "oo\n"
      expect(scanner.lineno).to eq 2
    end
  end

  describe "#terminate" do
    it "skips to end of input" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("foo\n"))
      scanner.terminate
      expect(scanner).to be_eos
    end
  end
end