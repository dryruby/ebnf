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
task default: :spec
task specs: :spec

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

namespace :etc do
  ETC_FILES = %w{
    etc/abnf.sxp etc/iso-ebnf.sxp
    etc/ebnf.sxp etc/ebnf.ll1.sxp etc/ebnf.peg.sxp etc/ebnf.html etc/ebnf.ll1.rb etc/ebnf.peg.rb
    etc/turtle.sxp
    etc/sparql.sxp
  }
  desc 'Remove generated files in etc'
  task :clean do
    %x(rm #{ETC_FILES.join(' ')})
  end

  desc 'Create versions of ebnf files in etc'
  task build: ETC_FILES
end

desc "Build meta files for ABNF, EBNF and ISO EBNF"
task :meta => %w{lib/ebnf/ebnf/meta.rb lib/ebnf/isoebnf/meta.rb lib/ebnf/abnf/meta.rb lib/ebnf/abnf/core.rb}

file "lib/ebnf/abnf/meta.rb" => "etc/abnf.ebnf" do
  %x(bin/ebnf --peg -f rb --mod-name ABNFMeta -o lib/ebnf/abnf/meta.rb etc/abnf.ebnf)
end

file "lib/ebnf/abnf/core.rb" => "etc/abnf-core.ebnf" do
  %x(bin/ebnf -f rb --mod-name ABNFCore -o lib/ebnf/abnf/core.rb etc/abnf-core.ebnf)
end

file "lib/ebnf/ebnf/meta.rb" => "etc/ebnf.peg.rb" do
  %x(cp etc/ebnf.peg.rb lib/ebnf/ebnf/meta.rb)
end

file "lib/ebnf/isoebnf/meta.rb" => "etc/iso-ebnf.ebnf" do
  %x(bin/ebnf --peg -f rb --mod-name ISOEBNFMeta -o lib/ebnf/isoebnf/meta.rb etc/iso-ebnf.ebnf)
end


# Build SXP output with leading space to allow for Markdown formatting.
rule ".sxp" => %w{.ebnf} do |t|
  puts "build #{t.name}"
  File.open(t.name, "w") do |f|
    IO.popen(%(bin/ebnf #{t.source})).each_line do |line|
      f.puts '    ' + line
    end
  end
end

rule ".peg.sxp" => %w{.ebnf} do |t|
  puts "build #{t.name}"
  File.open(t.name, "w") do |f|
    IO.popen(%(bin/ebnf --peg #{t.source})).each_line do |line|
      f.puts '    ' + line
    end
  end
end

rule ".html" => %w{.ebnf} do |t|
  puts "build #{t.name}"
  %x(bin/ebnf --format html -o #{t.name} #{t.source})
end

file "etc/ebnf.ll1.sxp" => "etc/ebnf.ebnf" do |t|
  puts "build #{t.name}"
  File.open(t.name, "w") do |f|
    IO.popen(%(bin/ebnf --ll1 ebnf #{t.source})).each_line do |line|
      f.puts '    ' + line
    end
  end
end

file "etc/ebnf.peg.rb" => "etc/ebnf.ebnf" do |t|
  puts "build #{t.name}"
  %x(bin/ebnf --peg --mod-name EBNFMeta -f rb -o etc/ebnf.peg.rb etc/ebnf.ebnf)
end

file "etc/ebnf.ll1.rb" => "etc/ebnf.ebnf" do |t|
  puts "build #{t.name}"
  %x(bin/ebnf --ll1 ebnf -f rb -o etc/ebnf.ll1.rb etc/ebnf.ebnf)
end
