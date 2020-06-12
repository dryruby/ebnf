$:.unshift(File.expand_path("../../lib", __FILE__))
$:.unshift File.dirname(__FILE__)

require 'bundler/setup'
require 'rdf/spec'
require 'rdf/spec/matchers'
require 'rspec'
require 'rspec/its'
require 'matchers'
begin
  require 'simplecov'
  require 'coveralls'
  SimpleCov.formatter = SimpleCov::Formatter::MultiFormatter.new([
    SimpleCov::Formatter::HTMLFormatter,
    Coveralls::SimpleCov::Formatter
  ])
  SimpleCov.start do
    add_filter "/spec/"
  end
rescue LoadError => e
  STDERR.puts "Coverage Skipped: #{e.message}"
end

::RSpec.configure do |c|
  c.filter_run focus: true
  c.run_all_when_everything_filtered = true
  c.exclusion_filter = {
    ruby: lambda { |version| !(RUBY_VERSION.to_s =~ /^#{version.to_s}/) },
    not_jruby: lambda { RUBY_PLATFORM.to_s != 'jruby'}
  }
end
