require 'rspec/expectations'
require_relative 'http_helpers'
require_relative 'payloads'

World(RSpec::Matchers, HttpHelpers, ContractPayloads)

Before do
  @headers = { 'Accept-Encoding' => 'identity' }
  @chunked = false
end

Before('@v2') do
  @headers['Accept-Encoding'] = 'gzip, deflate, br'
end

AfterStep('@v2') do
  if @response
    expect(@response['Content-Encoding']).to be_nil
    expect(@response.instance_variable_get(:@decode_content)).to be_falsey
  end
end
