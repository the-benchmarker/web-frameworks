require_relative '../spec_helper'

RSpec.describe 'routing' do
  %w[identity gzip,br,zstd].each do |encoding|
    it "serves an empty health response with Accept-Encoding: #{encoding}" do
      expect_empty_response(send_request('GET', '/', encoding: encoding))
    end

    it "dispatches POST /user with Accept-Encoding: #{encoding}" do
      expect_empty_response(send_request('POST', '/user', body: '', encoding: encoding))
    end
  end

  %w[00000000 00000042 12345678 99999999].each do |id|
    %w[identity gzip,br,zstd].each do |encoding|
      it "returns the string ID #{id} with Accept-Encoding: #{encoding}" do
        response = send_request('GET', "/user/#{id}", encoding: encoding)

        expect_json_response(response, { 'id' => id })
      end
    end
  end

  it 'decodes the path parameter before validating the ID' do
    response = send_request('GET', '/user/%30%30%30%30%30%30%34%32')

    expect_json_response(response, { 'id' => '00000042' })
  end

  %w[0 0000042 000000042 abcdefgh 1234abcd 12345.67 1234567%0A 1234567%C3%A9].each do |id|
    it "rejects an invalid path ID: #{id}" do
      response = send_request('GET', "/user/#{id}", encoding: 'gzip, br, zstd')

      expect([400, 404]).to include(response.code.to_i)
    end
  end

  it 'uses the route ID when a query parameter has the same name' do
    response = send_request('GET', '/user/00000042?id=99999999')

    expect_json_response(response, { 'id' => '00000042' })
  end

  it 'uses the route ID when the JSON body contains another ID' do
    response = send_request('GET', '/user/00000042', body: JSON.generate(id: '99999999'),
                                                     content_type: 'application/json')

    expect_json_response(response, { 'id' => '00000042' })
  end
end
