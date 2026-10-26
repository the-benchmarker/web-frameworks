# frozen_string_literal: true

require 'net/http'
require 'yaml'

module HttpDiagnostics
  BODY_LIMIT = 8192

  def perform_request(request)
    @http_request = request
    started_at = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    @http_client = http
    @http_response = @http_client.request(request)
  rescue StandardError => e
    @http_error = e
    raise
  ensure
    @http_elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - started_at
  end

  def http_diagnostics
    target = @http_request.path
    target = "http://#{@http_client.address}:#{@http_client.port}#{target}" if @http_client
    lines = [
      "Framework: #{ENV.fetch('LANGUAGE', nil)}/#{ENV.fetch('FRAMEWORK', nil)} " \
      "(engine: #{ENV.fetch('ENGINE', nil)})",
      "Request: #{@http_request.method} #{target}",
      http_message_details(@http_request)
    ]

    if @http_response
      lines << "Response: HTTP/#{@http_response.http_version} #{@http_response.code} #{@http_response.message}"
      lines << http_message_details(@http_response)
    else
      lines << 'Response: no HTTP response received'
      lines << "Error: #{@http_error.class}: #{@http_error.message}" if @http_error
    end

    lines << format('Elapsed: %.1f ms', @http_elapsed * 1000)
    lines.join("\n")
  end

  private

  def http_message_details(message)
    lines = message.to_hash.flat_map do |name, values|
      values.map { |value| "#{name}: #{value}" }
    end
    body = message.body.to_s
    lines << "Body (#{body.bytesize} bytes): #{body.byteslice(0, BODY_LIMIT).inspect}"
    lines << "[body truncated to #{BODY_LIMIT} bytes]" if body.bytesize > BODY_LIMIT
    lines.join("\n")
  end
end

RSpec.configure do |config|
  config.include HttpDiagnostics, :http

  config.expect_with :rspec do |expectations|
    expectations.include_chain_clauses_in_custom_matcher_descriptions = true
  end

  config.mock_with :rspec do |mocks|
    mocks.verify_partial_doubles = true
  end

  config.shared_context_metadata_behavior = :apply_to_host_groups

  config.after(:each, :http) do |example|
    next unless ENV['CI'] == 'true' && example.exception && @http_request

    RSpec.configuration.reporter.message("HTTP diagnostics for #{example.full_description}\n#{http_diagnostics}")
  end
end

def http
  language = ENV.fetch('LANGUAGE', nil)
  framework = ENV.fetch('FRAMEWORK', nil)
  engine = ENV.fetch('ENGINE', nil)

  ip = File.read(File.join(language, framework, "ip-#{engine}.txt")).strip
  Net::HTTP.new(ip, 3000)
end
