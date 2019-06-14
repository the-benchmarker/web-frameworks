# frozen_string_literal: true

require "rack"
require "rspec"
require "rack/test"

require "./app/init"

RSpec.configure do |config|
  config.filter_run focus: true
  config.run_all_when_everything_filtered = true
  config.color = true

  include Rack::Test::Methods

  config.backtrace_exclusion_patterns = [
    %r{/lib\d*/ruby/},
    %r{bin/},
    /gems/,
    %r{spec/spec_helper\.rb},
    %r{lib/rspec/(core|expectations|matchers|mocks)}
  ]

  config.before(:each) do
  end
end
