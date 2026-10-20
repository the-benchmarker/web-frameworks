require_relative 'spec_helper'

RSpec.describe 'legacy routes', :legacy do
  context 'GET : /' do
    subject(:response) { send_request('GET', '/') }

    it 'returns successfully' do
      expect(response).to be_a(Net::HTTPSuccess)
    end

    it 'returns an empty string' do
      expect(response.body.to_s).to be_empty
    end
  end

  context 'GET : /user/0' do
    subject(:response) { send_request('GET', '/user/0') }

    it 'returns successfully' do
      expect(response).to be_a(Net::HTTPSuccess)
    end

    it 'returns the expected param' do
      expect(response.body.to_s).to eq('0')
    end
  end

  context 'POST : /user' do
    subject(:response) { send_request('POST', '/user', body: '', content_type: 'text/plain') }

    it 'returns successfully' do
      expect(response).to be_a(Net::HTTPSuccess)
    end

    it 'returns an empty string' do
      expect(response.body.to_s).to be_empty
    end
  end

  [['GET', '/', ''], ['GET', '/user/0', '0'], ['POST', '/user', '']].each do |method, path, body|
    it "keeps #{method} #{path} uncompressed when compression is advertised" do
      response = send_request(method, path, body: method == 'POST' ? '' : nil, encoding: 'gzip, br, zstd')

      expect(response).to be_a(Net::HTTPSuccess)
      expect(response.body.to_s).to eq(body)
    end
  end
end
