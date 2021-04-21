#!/usr/bin/env ruby -rubygems
# -*- encoding: utf-8 -*-

Gem::Specification.new do |gem|
  gem.version               = File.read('VERSION').chomp
  gem.date                  = File.mtime('VERSION').strftime('%Y-%m-%d')

  gem.name                  = "ebnf"
  gem.homepage              = "https://github.com/dryruby/ebnf"
  gem.license               = 'Unlicense'
  gem.summary               = "EBNF parser and parser generator in Ruby."
  gem.description           = %q{EBNF is a Ruby parser for W3C EBNF and a parser generator for PEG and LL(1). Also includes parsing modes for ISO EBNF and ABNF.}

  gem.authors               = ['Gregg Kellogg']
  gem.email                 = 'public-rdf-ruby@w3.org'

  gem.platform              = Gem::Platform::RUBY
  gem.files                 = %w(AUTHORS CREDITS README.md UNLICENSE VERSION) +
                            Dir.glob('lib/**/*.rb') +
                            Dir.glob('etc/*')
  gem.bindir               = %q(bin)
  gem.executables          = %w(ebnf)
  gem.require_paths         = %w(lib)

  gem.required_ruby_version = '>= 2.4'
  gem.requirements          = []
  gem.add_runtime_dependency     'sxp',             '~> 1.1'
  gem.add_runtime_dependency     'scanf',           '~> 1.0'
  gem.add_runtime_dependency     'rdf',             '~> 3.1' # Required by sxp
  gem.add_runtime_dependency     'htmlentities',    '~> 4.3'
  gem.add_runtime_dependency     'unicode-types',   '~> 1.6'
  gem.add_runtime_dependency     'amazing_print',   '~> 1.2'
  gem.add_development_dependency 'rdf-spec',        '~> 3.1'
  gem.add_development_dependency 'rdf-turtle',      '~> 3.1'
  gem.add_development_dependency 'nokogiri',        '~> 1.10'
  gem.add_development_dependency 'erubis',          '~> 2.7'
  gem.add_development_dependency 'rspec',           '~> 3.10'
  gem.add_development_dependency 'rspec-its',       '~> 1.3'
  gem.add_development_dependency 'yard',            '~> 0.9'
  gem.add_development_dependency 'rake',            '~> 13.0'

  gem.post_install_message  = nil
end
