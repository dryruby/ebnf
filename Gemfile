source "https://rubygems.org"

gemspec :name => ""
gem 'sxp',            github: "gkellogg/sxp-ruby",  branch: "develop"
gem 'rdf',            github: "ruby-rdf/rdf",       branch: "develop"

group :development do
  gem "wirble"
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
