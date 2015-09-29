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
      scanner = EBNF::LL1::Scanner.new("line1\nline2\n")
      expect(scanner.rest).to eq "line1\nline2\n"
      expect(scanner).not_to be_eos
    end
    
    it "encodes input to UTF-8", :pending => !"".respond_to?(:force_encoding) do
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
  
  describe "#rest" do
    it "returns remaining scanner contents if not at eos" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("foo\n"))
      expect(scanner.rest).to eq "foo\n"
    end
    
    it "returns next line from file if at eos" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("\nfoo\n"))
      expect(scanner.rest).to eq "\nfoo\n"
      scanner.scan(/\s*/m)
      expect(scanner.rest).to eq "foo\n"
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
      end
      
      it "returns a STRING_LITERAL_QUOTE" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("'string' foo"))
        expect(scanner.scan(/'((?:[^\x27\x5C\x0A\x0D])*)'/)).to eq "'string'"
      end
      
      it "returns a STRING_LITERAL_LONG_SINGLE_QUOTE" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("'''\nstring\nstring''' foo"))
        expect(scanner.scan(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m)).to eq "'''\nstring\nstring'''"
      end
      
      it "scans a multi-line string" do
         string = %q('''
          <html:a="b"/>
          '''
        )
        scanner = EBNF::LL1::Scanner.new(StringIO.new(string))
        expect(scanner.scan(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m)).not_to be_empty
      end
      
      it "scans a longer multi-line string" do
         string = %q('''
          <html:b xmlns:html="http://www.w3.org/1999/xhtml" html:a="b"/>
          '''
        )
        scanner = EBNF::LL1::Scanner.new(StringIO.new(string))
        expect(scanner.scan(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m)).not_to be_empty
      end
    end
  end
  
  describe "#skip" do
    it "skips input" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("foo\n"))
      scanner.skip(/^f/)
      expect(scanner.rest).to eq "oo\n"
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