require_relative '../spec_helper'

# The legacy contract allows any 2xx status and returns the route ID as text.
RSpec.describe 'v1 HTTP routes' do
  %w[identity gzip,deflate,br,zstd].each do |encoding|
    context "with Accept-Encoding: #{encoding}" do
      [['GET', '/'], ['POST', '/user']].each do |method, path|
        it "serves an empty successful response to #{method} #{path}" do
          response = send_request(method, path, body: method == 'POST' ? '' : nil, encoding: encoding)

          expect(response).to be_a(Net::HTTPSuccess)
          expect(response.body.to_s).to eq('')
        end
      end

      %w[0 42 00000042 123456789].each do |id|
        it "returns the plain route ID #{id}" do
          response = send_request('GET', "/user/#{id}", encoding: encoding)

          expect(response).to be_a(Net::HTTPSuccess)
          expect(response.body).to eq(id)
        end
      end
    end
  end

  it 'reads the ID from the path when a query parameter has the same name' do
    response = send_request('GET', '/user/42?id=other')

    expect(response).to be_a(Net::HTTPSuccess)
    expect(response.body).to eq('42')
  end

  it 'uses the current path on successive requests' do
    %w[42 00000042 0 42].each do |id|
      response = send_request('GET', "/user/#{id}")

      expect(response).to be_a(Net::HTTPSuccess)
      expect(response.body).to eq(id)
    end
  end
end
