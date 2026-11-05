require_relative '../spec_helper'

RSpec.describe 'routing' do
  it 'serves the home page with an empty response' do
    expect_empty_response(send_request('GET', '/'))
  end

  it 'accepts a request to create a user with an empty response' do
    expect_empty_response(send_request('POST', '/user', body: ''))
  end

  %w[00000000 00000042 12345678 99999999].each do |id|
    it "returns the requested ID #{id}" do
      expect_json_response(send_request('GET', "/user/#{id}"), { 'id' => id })
    end
  end

  {
    '%30%30%30%30%30%30%34%32' => '00000042',
    '000000%34%32' => '00000042'
  }.each do |path_id, id|
    it "accepts the valid ID #{id} when parts of the path are percent-encoded" do
      expect_json_response(send_request('GET', "/user/#{path_id}"), { 'id' => id })
    end
  end

  %w[0 0000042 000000042 abcdefgh 1234abcd 12345.67 -1234567 +1234567
     1234567%20 1234567%0A 12345678%0A %0A12345678 1234567%C3%A9
     %D9%A0%D9%A1%D9%A2%D9%A3%D9%A4%D9%A5%D9%A6%D9%A7].each do |id|
    it "rejects the invalid ID #{id}" do
      response = send_request('GET', "/user/#{id}")

      expect([400, 404]).to include(response.code.to_i)
    end
  end

  ['?id=99999999', '?id[]=99999999'].each do |query|
    it "uses the path ID even when the query includes #{query}" do
      expect_json_response(send_request('GET', "/user/00000042#{query}"), { 'id' => '00000042' })
    end
  end

  it 'uses the path ID when the request body includes a different ID' do
    response = send_request('GET', '/user/00000042', body: JSON.generate(id: '99999999'),
                                                     content_type: 'application/json')

    expect_json_response(response, { 'id' => '00000042' })
  end

  it 'rejects an invalid path ID even when the query and body include a valid ID' do
    response = send_request('GET', '/user/invalid?id=00000042', body: JSON.generate(id: '00000042'),
                                                                content_type: 'application/json')

    expect([400, 404]).to include(response.code.to_i)
  end

  it 'returns the current ID for each request' do
    %w[00000042 99999999 00000000 00000042].each do |id|
      expect_json_response(send_request('GET', "/user/#{id}"), { 'id' => id })
    end
  end
end
