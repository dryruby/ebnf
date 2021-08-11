$:.unshift(File.expand_path("../../lib", __FILE__))
$:.unshift File.dirname(__FILE__)

require 'bundler/setup'
require 'amazing_print'
require 'rdf/spec'
require 'rdf/spec/matchers'
require 'rspec'
require 'rspec/matchers'
require 'rspec/its'
require 'matchers'

begin
  require 'simplecov'
  require 'simplecov-lcov'

  SimpleCov::Formatter::LcovFormatter.config do |config|
    #Coveralls is coverage by default/lcov. Send info results
    config.report_with_single_file = true
    config.single_report_path = 'coverage/lcov.info'
  end

  SimpleCov.formatter = SimpleCov::Formatter::MultiFormatter.new([
    SimpleCov::Formatter::HTMLFormatter,
    SimpleCov::Formatter::LcovFormatter
  ])
  SimpleCov.start do
    add_filter "/spec/"
  end
rescue LoadError => e
  STDERR.puts "Coverage Skipped: #{e.message}"
end

require 'ebnf'

::RSpec.configure do |c|
  c.filter_run focus: true
  c.run_all_when_everything_filtered = true
  c.filter_run_excluding ruby: ->(version) do
    case version.to_s
    when "!jruby"
      RUBY_ENGINE == "jruby"
    when /^> (.*)/
      !(RUBY_VERSION.to_s > $1)
    else
      !(RUBY_VERSION.to_s =~ /^#{version.to_s}/)
    end
  end
end

RSpec::Matchers.define :be_valid_html do
  match do |actual|
    return true unless Nokogiri.const_defined?(:HTML5)
    root = Nokogiri::HTML5("<!DOCTYPE html>" + actual, max_parse_errors: 1000)
    @errors = Array(root && root.errors.map(&:to_s))
    @errors.empty?
  end
  
  failure_message do |actual|
    "expected no errors, was #{@errors.join("\n")}\n" + actual
  end
end

PARSED_EBNF_GRAMMAR = EBNF.parse(File.open(File.expand_path("../../etc/ebnf.ebnf", __FILE__)), format: :native).freeze