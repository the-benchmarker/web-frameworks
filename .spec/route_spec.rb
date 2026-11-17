# frozen_string_literal: true

require 'net/http'

require_relative 'spec_helper'

RSpec.describe 'routes' do
  context 'GET : /' do
    subject(:response) { http.request(Net::HTTP::Get.new('/', 'Accept-Encoding' => 'identity')) }

    it 'returns successfully' do
      expect(response).to be_a(Net::HTTPSuccess)
    end

    it 'returns an empty string' do
      expect(response.body.to_s).to be_empty
    end
  end

  context 'GET : /user/0' do
    subject(:response) { http.request(Net::HTTP::Get.new('/user/0', 'Accept-Encoding' => 'identity')) }

    it 'returns successfully' do
      expect(response).to be_a(Net::HTTPSuccess)
    end

    it 'returns the expected param' do
      expect(response.body.to_s).to eq('0')
    end
  end

  context 'POST : /user' do
    let!(:request) { Net::HTTP::Post.new('/user', 'Accept-Encoding' => 'identity') }

    before { request['Content-Type'] = 'text/plain' }

    subject(:response) { http.request(request) }

    it 'returns successfully' do
      expect(response).to be_a(Net::HTTPSuccess)
    end

    it 'returns an empty string' do
      expect(response.body.to_s).to be_empty
    end
  end
end
