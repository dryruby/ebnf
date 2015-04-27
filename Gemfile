source "https://rubygems.org"

gemspec :name => ""
gem 'sxp',            git: "git://github.com/gkellogg/sxp-ruby.git"

group :debug do
  gem "wirble"
  gem "byebug", platform: :mri_21
  gem "rocco", platforms: :mri, git: "git://github.com/rtomayko/rocco.git"
  gem "redcarpet", platforms: :mri
end

platforms :rbx do
  gem 'rubysl', '~> 2.0'
  gem 'rubinius', '~> 2.0'
end
