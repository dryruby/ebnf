#!/usr/bin/env ruby -rubygems
# -*- encoding: utf-8 -*-

Gem::Specification.new do |gem|
  gem.version               = File.read('VERSION').chomp
  gem.date                  = File.mtime('VERSION').strftime('%Y-%m-%d')

  gem.name                  = "ebnf"
  gem.homepage              = "http://github.com/gkellogg/ebnf"
  gem.license               = 'Public Domain' if gem.respond_to?(:license=)
  gem.summary               = "EBNF parser and parser generator."
  gem.description           = %q{EBNF is a Ruby parser for W3C EBNF and a parser generator for compliant LL(1) grammars.}

  gem.authors               = ['Gregg Kellogg']
  gem.email                 = 'public-rdf-ruby@w3.org'

  gem.platform              = Gem::Platform::RUBY
  gem.files                 = %w(AUTHORS CREDITS README.md UNLICENSE VERSION) +
                            Dir.glob('lib/**/*.rb') +
                            Dir.glob('etc/*')
  gem.bindir               = %q(bin)
  gem.executables          = %w(ebnf)
  gem.default_executable   = gem.executables.first
  gem.require_paths         = %w(lib)
  gem.extensions            = %w()
  gem.test_files            = %w()
  gem.has_rdoc              = false

  gem.required_ruby_version = '>= 2.0'
  gem.requirements          = []
  gem.add_runtime_dependency     'sxp',             '~> 0.1', '>= 0.1.3'
  gem.add_runtime_dependency     'rdf',             '>= 1.99', '< 3' # Required by sxp
  gem.add_development_dependency 'haml',            '~> 4.0'
  gem.add_development_dependency 'rspec',           '~> 3.0'
  gem.add_development_dependency 'rspec-its',       '~> 1.0'
  gem.add_development_dependency 'yard',            '~> 0.8'
  gem.add_development_dependency 'rake',            '~> 10.4'

  gem.post_install_message  = nil
end
