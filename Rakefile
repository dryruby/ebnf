#!/usr/bin/env ruby
$:.unshift(File.expand_path(File.join(File.dirname(__FILE__), 'lib')))
require 'rubygems'

namespace :gem do
  desc "Build the ebnf-#{File.read('VERSION').chomp}.gem file"
  task :build do
    sh "gem build ebnf.gemspec && mv ebnf-#{File.read('VERSION').chomp}.gem pkg/"
  end

  desc "Release the ebnf-#{File.read('VERSION').chomp}.gem file"
  task :release do
    sh "gem push pkg/ebnf-#{File.read('VERSION').chomp}.gem"
  end
end

desc 'Default: run specs.'
task :default => :spec
task :specs => :spec

require 'rspec/core/rake_task'
desc 'Run specifications'
RSpec::Core::RakeTask.new do |spec|
  spec.rspec_opts = %w(--options spec/spec.opts) if File.exists?('spec/spec.opts')
end

desc "Run specs through RCov"
RSpec::Core::RakeTask.new("spec:rcov") do |spec|
  spec.rcov = true
  spec.rcov_opts =  %q[--exclude "spec"]
end

desc "Generate HTML report specs"
RSpec::Core::RakeTask.new("doc:spec") do |spec|
  spec.rspec_opts = ["--format", "html", "-o", "doc/spec.html"]
end

require 'yard'
namespace :doc do
  YARD::Rake::YardocTask.new
end

desc 'Create versions of ebnf files in etc'
task :etc => %w{etc/ebnf.sxp etc/ebnf.ll1.sxp etc/ebnf.html etc/ebnf.rb etc/turtle.sxp etc/turtle.ll1.sxp etc/turtle.rb}

rule ".sxp" => %w{.ebnf} do |t|
  puts "build #{t.name}"
  %x(bin/ebnf -o #{t.name} #{t.source})
end

file "etc/ebnf.ll1.sxp" => "etc/ebnf.ebnf" do
  %x(bin/ebnf --ll1 ebnf -o etc/ebnf.ll1.sxp etc/ebnf.ebnf)
end

file "etc/ebnf.rb" => "etc/ebnf.ebnf" do
  %x(bin/ebnf --ll1 ebnf -f rb -o etc/ebnf.rb etc/ebnf.ebnf)
end

file "etc/ebnf.html" => "etc/ebnf.ebnf" do
  %x(bin/ebnf --format html -o etc/ebnf.html etc/ebnf.ebnf)
end

file "etc/turtle.ll1.sxp" => "etc/turtle.ebnf" do
  %x(bin/ebnf --ll1 turtleDoc -o etc/turtle.ll1.sxp etc/turtle.ebnf)
end

file "etc/turtle.rb" => "etc/turtle.ebnf" do
  %x(bin/ebnf --ll1 turtleDoc -f rb -o etc/turtle.rb etc/turtle.ebnf)
end
