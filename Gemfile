source "https://rubygems.org"

gemspec
gem 'sxp',            github: "dryruby/sxp.rb",     branch: "develop"
gem 'rdf',            github: "ruby-rdf/rdf",       branch: "develop"

group :development do
  gem 'rdf-spec',   github: "ruby-rdf/rdf-spec",    branch: "develop"
  gem "byebug",     platforms: :mri
  gem 'psych',      platforms: [:mri, :rbx]
  gem "redcarpet",  platforms: :mri
  gem "rocco"
  gem "pygmentize"
end

group :development, :test do
  gem 'simplecov',  platforms: :mri
  gem 'coveralls',  '~> 0.8', platforms: :mri
  gem 'awesome_print',    github: 'MatthiasWinkelmann/awesome_print'
end
