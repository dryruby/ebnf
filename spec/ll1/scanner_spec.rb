# coding: utf-8
$:.unshift ".."
require 'spec_helper'
require 'ebnf'

describe EBNF::LL1::Scanner do
  describe ".new" do
    it "initializes with a StringIO" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("line1\nline2\n"))
      scanner.rest.should == "line1\nline2\n"
      scanner.eos?.should be_false
    end
    
    it "initializes with a string" do
      scanner = EBNF::LL1::Scanner.new("line1\nline2\n")
      scanner.rest.should == "line1\nline2\n"
      scanner.eos?.should be_false
    end
    
    it "encodes input to UTF-8", :pending => !"".respond_to?(:force_encoding) do
      f = mock("input")
      f.should_receive(:read).and_return("ascii".force_encoding(Encoding::ASCII_8BIT))
      f.should_receive(:gets).and_return("utf8".force_encoding(Encoding::UTF_8))
      f.should_receive(:eof?).and_return(false, false, true)
      scanner = EBNF::LL1::Scanner.new(f)
      s = scanner.rest
      s.should == "asciiutf8"
      s.encoding.should == Encoding::UTF_8
    end
  end
  
  describe "#eos?" do
    it "returns true if at both eos and eof" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new(""))
      scanner.eos?.should be_true
    end
  end
  
  describe "#rest" do
    it "returns remaining scanner contents if not at eos" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("foo\n"))
      scanner.rest.should == "foo\n"
    end
    
    it "returns next line from file if at eos" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new("\nfoo\n"))
      scanner.rest.should == "\nfoo\n"
      scanner.scan(/\s*/m)
      scanner.rest.should == "foo\n"
    end
    
    it "returns \"\" if at eos and eof" do
      scanner = EBNF::LL1::Scanner.new(StringIO.new(""))
      scanner.rest.should == ""
    end
  end
  
  describe "#scan" do
    context "simple terminals" do
      it "returns a word" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("foo bar"))
        scanner.scan(/\w+/).should == "foo"
      end
      
      it "returns a STRING_LITERAL_QUOTE" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("'string' foo"))
        scanner.scan(/'((?:[^\x27\x5C\x0A\x0D])*)'/).should == "'string'"
      end
      
      it "returns a STRING_LITERAL_LONG_SINGLE_QUOTE" do
        scanner = EBNF::LL1::Scanner.new(StringIO.new("'''\nstring\nstring''' foo"))
        scanner.scan(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m).should == "'''\nstring\nstring'''"
      end
      
      it "scans a multi-line string" do
         string = %q('''
          <html:a="b"/>
          '''
        )
        scanner = EBNF::LL1::Scanner.new(StringIO.new(string))
        scanner.scan(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m).should_not be_empty
      end
      
      it "scans a longer multi-line string" do
         string = %q('''
          <html:b xmlns:html="http://www.w3.org/1999/xhtml" html:a="b"/>
          '''
        )
        scanner = EBNF::LL1::Scanner.new(StringIO.new(string))
        scanner.scan(/'''((?:(?:'|'')?(?:[^'\\])+)*)'''/m).should_not be_empty
      end
    end
  end
end