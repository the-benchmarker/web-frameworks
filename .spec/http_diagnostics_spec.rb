# frozen_string_literal: true

require_relative 'spec_helper'

RSpec.describe 'HTTP request diagnostics', :http do
  let(:client) { instance_double(Net::HTTP, address: '127.0.0.1', port: 3000) }
  let(:response) { instance_double(Net::HTTPOK) }

  before { allow(self).to receive(:http).and_return(client) }

  context 'GET' do
    it 'makes the request helper available in nested route examples' do
      request = Net::HTTP::Get.new('/user/0')
      allow(client).to receive(:request).with(request).and_return(response)

      expect(perform_request(request)).to be(response)
    end
  end

  context 'POST' do
    it 'preserves the request headers and body' do
      request = Net::HTTP::Post.new('/user')
      request['Content-Type'] = 'text/plain'
      request.body = 'example'
      allow(client).to receive(:request).with(request) do |sent|
        expect(sent['Content-Type']).to eq('text/plain')
        expect(sent.body).to eq('example')
        response
      end

      expect(perform_request(request)).to be(response)
    end
  end

  it 'preserves connection errors and records details for diagnostics' do
    request = Net::HTTP::Get.new('/')
    error = Errno::ECONNREFUSED.new('connection refused')
    allow(client).to receive(:request).with(request).and_raise(error)

    expect { perform_request(request) }.to raise_error { |raised| expect(raised).to be(error) }
    expect(http_diagnostics).to include(
      'Request: GET http://127.0.0.1:3000/',
      'Response: no HTTP response received',
      "Error: #{error.class}: #{error.message}",
      'Elapsed:'
    )
  end
end
