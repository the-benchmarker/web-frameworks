# frozen_string_literal: true

require 'net/http'
require 'yaml'

RSpec.configure do |config|
  config.expect_with :rspec do |expectations|
    expectations.include_chain_clauses_in_custom_matcher_descriptions = true
  end

  config.mock_with :rspec do |mocks|
    mocks.verify_partial_doubles = true
  end

  config.shared_context_metadata_behavior = :apply_to_host_groups
end

def address(name)
  language, framework = name.split('/')
  ip = File.read(File.join(language, framework, 'ip.txt')).strip
  Net::HTTP.new(ip, 3000)
end
