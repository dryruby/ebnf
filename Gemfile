source "https://rubygems.org"

gemspec :name => ""
gem 'sxp',            git: "git://github.com/gkellogg/sxp-ruby.git", branch: "develop"
gem 'rdf',            git: "git://github.com/ruby-rdf/rdf.git", branch: "develop"

group :development do
  gem "wirble"
  gem "byebug", platforms: :mri
  gem 'psych',      platforms: [:mri, :rbx]
  gem "rocco", platforms: :mri, git: "git://github.com/rtomayko/rocco.git"
  gem "redcarpet", platforms: :mri
end

group :development, :test do
  gem 'simplecov',  require: false
  gem 'coveralls',  require: false
end

platforms :rbx do
  gem 'rubysl', '~> 2.0'
  gem 'rubinius', '~> 2.0'
end
