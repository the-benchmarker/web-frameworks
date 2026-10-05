# frozen_string_literal: true

require 'digest'
require 'json'
require 'net/http'

require_relative 'spec_helper'

RSpec.describe 'Rails scenario edge cases', :v2 do
  before do
    skip 'Rails-only contract' unless ENV['LANGUAGE'] == 'ruby' && ENV['FRAMEWORK'] == 'rails'
  end

  def get(path)
    http.request(Net::HTTP::Get.new(path))
  end

  def post_json(path, value)
    request = Net::HTTP::Post.new(path)
    request['Content-Type'] = 'application/json'
    request.body = value.is_a?(String) ? value : JSON.generate(value)
    http.request(request)
  end

  it 'decodes UTF-8 path and query values' do
    expect(get('/user/%E2%98%83').body.force_encoding('UTF-8')).to eq('☃')
    response = get('/serialization?n=2&seed=caf%C3%A9')

    expect(response.code).to eq('200')
    expect(JSON.parse(response.body)).to eq(
      'items' => [
        { 'id' => 0, 'value' => 'café:0' },
        { 'id' => 1, 'value' => 'café:1' }
      ]
    )
  end

  it 'accepts the serialization upper bound and rejects array parameters' do
    response = get('/serialization?n=1000&seed=limit')
    items = JSON.parse(response.body).fetch('items')

    expect(response.code).to eq('200')
    expect(items.length).to eq(1000)
    expect(items.last).to eq('id' => 999, 'value' => 'limit:999')
    expect(get('/serialization?n[]=2&seed=bad').code).to eq('400')
    expect(get('/serialization?n=2&seed[]=bad').code).to eq('400')
  end

  it 'checksums validated string values in order and permits empty items' do
    values = ['é', "line\nbreak", '']
    response = post_json('/deserialization', 'items' => values.map { |value| { 'value' => value } })

    expect(response.code).to eq('200')
    expect(JSON.parse(response.body)).to eq(
      'count' => 3,
      'checksum' => Digest::SHA256.hexdigest(values.join("\n"))
    )

    empty = post_json('/deserialization', 'items' => [])
    expect(empty.code).to eq('200')
    expect(JSON.parse(empty.body)).to eq(
      'count' => 0,
      'checksum' => Digest::SHA256.hexdigest('')
    )
  end

  it 'rejects malformed or invalid deserialization payloads' do
    ['{broken', '{}', '{"items":{}}', '{"items":[{}]}',
     '{"items":[{"value":2}]}', '{"items":[{"value":"ok"},null]}'].each do |body|
      expect(post_json('/deserialization', body).code).to eq('400')
    end
  end

  it 'rejects a missing upload and disables response caching' do
    request = Net::HTTP::Post.new('/upload')
    request['Content-Type'] = 'multipart/form-data; boundary=empty'
    request.body = "--empty--\r\n"

    expect(http.request(request).code).to eq('400')
    expect(get('/heath')['Cache-Control']).to include('no-store')
    expect(get('/serialization?n=0&seed=x')['Cache-Control']).to include('no-store')
  end
end
