source "https://rubygems.org"

gemspec
gem 'sxp',            github: "dryruby/sxp.rb",     branch: "develop"
gem 'rdf',            github: "ruby-rdf/rdf",       branch: "develop"

group :development do
  gem 'rdf-spec',   github: "ruby-rdf/rdf-spec",    branch: "develop"
  gem "byebug",     platforms: :mri
  gem 'psych',      platforms: [:mri, :rbx]
  gem "rocco",      platforms: :mri, github: "rtomayko/rocco"
  gem "redcarpet",  platforms: :mri
end

group :development, :test do
  gem 'simplecov',  require: false
  gem 'coveralls',  require: false
end

platforms :rbx do
  gem 'rubysl', '~> 2.0'
  gem 'rubinius', '~> 2.0'
end
