require 'json'
require 'net/http'

require_relative 'spec_helper'

RSpec.describe 'benchmark scenario', :v2 do
  def expect_json(response)
    expect(response.code).to eq('200')
    expect(response['Content-Type']).to start_with('application/json')
    JSON.parse(response.body)
  end

  it 'routes GET /heath and returns an empty body' do
    response = http.request(Net::HTTP::Get.new('/heath'))

    expect(response.code).to eq('200')
    expect(response.body.to_s).to be_empty
  end

  it 'returns the dynamic user path value as plain text' do
    { '/user/alice' => 'alice', '/user/42' => '42' }.each do |path, value|
      response = http.request(Net::HTTP::Get.new(path))

      expect(response.code).to eq('200')
      expect(response['Content-Type']).to start_with('text/plain')
      expect(response.body).to eq(value)
    end
  end

  it 'keeps POST /user compatible with the existing route' do
    request = Net::HTTP::Post.new('/user')
    request['Content-Type'] = 'text/plain'
    request.body = ''
    response = http.request(request)

    expect(response.code).to eq('200')
    expect(response.body.to_s).to be_empty
  end

  it 'serializes the requested number of items from the seed' do
    response = http.request(Net::HTTP::Get.new('/serialization?n=2&seed=demo'))

    expect(expect_json(response)).to eq(
      'items' => [
        { 'id' => 0, 'value' => 'demo:0' },
        { 'id' => 1, 'value' => 'demo:1' }
      ]
    )
  end

  it 'supports an empty serialization result and a different seed' do
    empty = http.request(Net::HTTP::Get.new('/serialization?n=0&seed=demo'))
    other = http.request(Net::HTTP::Get.new('/serialization?n=3&seed=other'))

    expect(expect_json(empty)).to eq('items' => [])
    expect(expect_json(other)).to eq(
      'items' => (0..2).map { |id| { 'id' => id, 'value' => "other:#{id}" } }
    )
  end

  it 'rejects missing and out-of-range serialization parameters' do
    [
      '/serialization?n=-1&seed=demo',
      '/serialization?n=1001&seed=demo',
      '/serialization?n=2&seed=',
      '/serialization?seed=demo',
      '/serialization?n=2'
    ].each do |path|
      expect(http.request(Net::HTTP::Get.new(path)).code).to eq('400')
    end
  end

  it 'deserializes the fixed JSON and checksums the value fields' do
    request = Net::HTTP::Post.new('/deserialization')
    request['Content-Type'] = 'application/json'
    request.body = JSON.generate('items' => %w[alpha beta gamma].map { |value| { 'value' => value } })

    expect(expect_json(http.request(request))).to eq(
      'count' => 3,
      'checksum' => 'f3220283d05d1ff2ae350cfe9e0e367cb5aef46e10efb203c8a53c678e2218c8'
    )
  end

  it 'hashes only the file bytes in the fixed multipart upload' do
    boundary = 'benchmark-scenario-boundary'
    file = 'abcd'.b * 1024
    request = Net::HTTP::Post.new('/upload')
    request['Content-Type'] = "multipart/form-data; boundary=#{boundary}"
    request.body = [
      "--#{boundary}\r\n",
      "Content-Disposition: form-data; name=\"file\"; filename=\"payload.bin\"\r\n",
      "Content-Type: application/octet-stream\r\n\r\n",
      file,
      "\r\n--#{boundary}--\r\n"
    ].join.b

    expect(expect_json(http.request(request))).to eq(
      'sha256' => '1f91053dcf43206eb082c0962785d35d86d4f629345f8bff25be7394416db908'
    )
  end
end
