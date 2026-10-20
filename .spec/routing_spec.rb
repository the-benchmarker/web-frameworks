require_relative 'spec_helper'

RSpec.describe 'routing', :complete do
  %w[identity gzip,br,zstd].each do |encoding|
    it "serves an empty health response with Accept-Encoding: #{encoding}" do
      expect_empty_response(send_request('GET', '/health', encoding: encoding))
    end

    it "dispatches POST /user with Accept-Encoding: #{encoding}" do
      expect_empty_response(send_request('POST', '/user', body: '', encoding: encoding))
    end
  end

  {
    '0' => '0', '00000042' => '00000042', 'alice' => 'alice',
    'alice.smith' => 'alice.smith', 'caf%C3%A9' => 'café', 'a%20b' => 'a b', 'a%2Bb' => 'a+b'
  }.each do |path, expected|
    %w[identity gzip,br,zstd].each do |encoding|
      it "returns the decoded path #{path} with Accept-Encoding: #{encoding}" do
        response = send_request('GET', "/user/#{path}", encoding: encoding)

        expect_status(response, 200)
        expect(response.content_type).to eq('text/plain')
        expect(response.type_params['charset']&.downcase).to eq('utf-8')
        expect(response.body.dup.force_encoding(Encoding::UTF_8)).to eq(expected)
      end
    end
  end

  it 'extracts the user from the path when query parameters have the same name' do
    response = send_request('GET', '/user/00000042?user=other')

    expect_status(response, 200)
    expect(response.body).to eq('00000042')
  end
end
