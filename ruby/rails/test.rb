require 'securerandom'

ENV['RAILS_ENV'] ||= 'production'
ENV['SECRET_KEY_BASE'] ||= SecureRandom.hex(64)
require_relative 'config/environment'
require 'rack/mock'
require_relative '../../.spec/support/http_helpers'
require_relative '../../.spec/support/payloads'

fixtures = Object.new.extend(HttpHelpers, ContractPayloads)
client = Rack::MockRequest.new(Rails.application)
puts "Rails #{Rails.version} / Rack #{Rack.release}"

expected_statuses = { 'fixture' => 200, 'empty boundary' => 400, 'quoted empty boundary' => 400 }
passed = true
expected_statuses.each do |name, expected|
  body, content_type = fixtures.upload_payload(name)
  response = client.post('/upload', 'CONTENT_TYPE' => content_type, input: body)
  puts "#{content_type} -> HTTP #{response.status} (expected #{expected})"
  passed = false unless response.status == expected
end
exit passed
