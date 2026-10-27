# The benchmark selects production through config.yaml; let Rails commands choose
# their usual environment when RAILS_ENV is not explicitly set.
ENV['BUNDLE_GEMFILE'] ||= File.expand_path('../Gemfile', __dir__)

require 'bundler/setup'
