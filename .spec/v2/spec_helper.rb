# frozen_string_literal: true

require_relative '../spec_helper'

module V2Contract
  IMPLEMENTATIONS = [
    %w[ruby rails]
  ].freeze

  def self.enabled?
    ENV.key?('BENCHMARK_HOST') || IMPLEMENTATIONS.include?([ENV['LANGUAGE'], ENV['FRAMEWORK']])
  end
end

RSpec.configure do |config|
  config.before(:each, :v2) do
    skip 'v2 contract is not enabled for this implementation' unless V2Contract.enabled?
  end
end
