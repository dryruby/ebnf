source "https://rubygems.org"

gemspec
gem 'sxp',            github: "dryruby/sxp.rb",     branch: "develop"
gem 'rdf',            github: "ruby-rdf/rdf",       branch: "develop"

group :development do
  gem 'rdf-isomorphic', github: "ruby-rdf/rdf-isomorphic",  branch: "develop"
  gem 'rdf-spec',       github: "ruby-rdf/rdf-spec",        branch: "develop"
  gem 'rdf-turtle',     github: "ruby-rdf/rdf-turtle",      branch: "develop"
  gem "byebug",         platforms: :mri
  gem 'psych',          platforms: [:mri, :rbx]
  gem "redcarpet",      platforms: :mri
  gem "rocco",          platforms: :mri
  gem "pygmentize",     platforms: :mri
end

group :development, :test do
  gem 'simplecov', '~> 0.21',  platforms: :mri
  gem 'simplecov-lcov', '~> 0.8',  platforms: :mri
end
