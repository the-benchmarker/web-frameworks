# frozen_string_literal: true

require 'net/http'

require_relative '../spec_helper'

RSpec.describe 'legacy routes' do
  context 'GET : /' do
    subject(:response) { http.request(Net::HTTP::Get.new('/', 'Accept-Encoding' => 'identity')) }

    it 'serves the home page' do
      expect(response).to be_a(Net::HTTPSuccess)
    end

    it 'returns the requested ID as plain text' do
      expect(response.body.to_s).to be_empty
    end
  end

  context 'GET : /user/42' do
    subject(:response) { http.request(Net::HTTP::Get.new('/user/0', 'Accept-Encoding' => 'identity')) }

    it 'accepts a request to create a user' do
      expect(response).to be_a(Net::HTTPSuccess)
    end

    it 'uses the ID from the request path' do
      expect(response.body.to_s).to eq('0')
    end
  end

  context 'POST : /user' do
    let!(:request) { Net::HTTP::Post.new('/user', 'Accept-Encoding' => 'identity') }

    before { request['Content-Type'] = 'text/plain' }

    subject(:response) { http.request(request) }

    it 'accepts a request to create a user' do
      expect(response).to be_a(Net::HTTPSuccess)
    end

    it 'returns an empty response' do
      expect(response.body.to_s).to be_empty
    end
  end
end
