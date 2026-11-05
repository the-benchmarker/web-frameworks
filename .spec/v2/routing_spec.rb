require_relative '../spec_helper'

RSpec.describe 'v2 HTTP routing' do
  %w[identity gzip,deflate,br,zstd].each do |encoding|
    context "with Accept-Encoding: #{encoding}" do
      it 'serves GET / with exactly 200 and zero body bytes' do
        expect_empty_response(send_request('GET', '/', encoding: encoding))
      end

      it 'serves an empty POST /user with exactly 200 and zero body bytes' do
        expect_empty_response(send_request('POST', '/user', body: '', encoding: encoding))
      end

      %w[00000000 00000042 12345678 99999999].each do |id|
        it "returns only the string ID #{id}" do
          expect_json_response(send_request('GET', "/user/#{id}", encoding: encoding), { 'id' => id })
        end
      end
    end
  end

  {
    '%30%30%30%30%30%30%34%32' => '00000042',
    '000000%34%32' => '00000042'
  }.each do |path_id, id|
    it "decodes #{path_id} before validating the route ID" do
      expect_json_response(send_request('GET', "/user/#{path_id}"), { 'id' => id })
    end
  end

  %w[0 0000042 000000042 abcdefgh 1234abcd 12345.67 -1234567 +1234567
     1234567%20 1234567%0A 12345678%0A %0A12345678 1234567%C3%A9
     %D9%A0%D9%A1%D9%A2%D9%A3%D9%A4%D9%A5%D9%A6%D9%A7].each do |id|
    it "rejects invalid route ID #{id} with 400 or 404" do
      response = send_request('GET', "/user/#{id}")

      expect([400, 404]).to include(response.code.to_i)
    end
  end

  ['?id=99999999', '?id[]=99999999'].each do |query|
    it "does not let query #{query} replace the route ID" do
      expect_json_response(send_request('GET', "/user/00000042#{query}"), { 'id' => '00000042' })
    end
  end

  it 'does not let a JSON body replace the route ID' do
    response = send_request('GET', '/user/00000042', body: JSON.generate(id: '99999999'),
                                                     content_type: 'application/json')

    expect_json_response(response, { 'id' => '00000042' })
  end

  it 'does not let a valid query or body repair an invalid route ID' do
    response = send_request('GET', '/user/invalid?id=00000042', body: JSON.generate(id: '00000042'),
                                                                content_type: 'application/json')

    expect([400, 404]).to include(response.code.to_i)
  end

  it 'uses the current route ID on successive requests' do
    %w[00000042 99999999 00000000 00000042].each do |id|
      expect_json_response(send_request('GET', "/user/#{id}"), { 'id' => id })
    end
  end
end
