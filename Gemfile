source "https://rubygems.org"

gemspec
gem 'sxp',            github: "dryruby/sxp.rb",     branch: "develop"
gem 'rdf',            github: "ruby-rdf/rdf",       branch: "develop"

group :development do
  gem 'rdf-spec',   github: "ruby-rdf/rdf-spec",    branch: "develop"
  gem "byebug",     platforms: :mri
  gem 'psych',      platforms: [:mri, :rbx]
  gem "redcarpet",  platforms: :mri
  gem "rocco",      platforms: :mri
  gem "pygmentize", platforms: :mri
end

group :development, :test do
  gem 'simplecov', '~> 0.21',  platforms: :mri
  gem 'simplecov-lcov', '~> 0.8',  platforms: :mri
end
