# frozen_string_literal: true

require 'json'
require 'net/http'
require 'stringio'

require_relative 'spec_helper'

RSpec.describe 'Spring REST workload' do
  before do
    skip 'Spring-only contract' unless ENV['LANGUAGE'] == 'java' && ENV['FRAMEWORK'] == 'spring'
  end

  def get(path)
    http.request(Net::HTTP::Get.new(path))
  end

  def post(path, body, content_type)
    request = Net::HTTP::Post.new(path)
    request['Content-Type'] = content_type
    request.body = body
    http.request(request)
  end

  it 'parses the numeric user route' do
    response = get('/user/42')
    expect(response.code).to eq('200')
    expect(response.body).to eq('42')
  end

  it 'serves health JSON' do
    response = get('/health')
    expect(response.code).to eq('200')
    expect(JSON.parse(response.body)).to eq('status' => 'ok')
  end

  it 'parses the multipart upload' do
    boundary = 'web-frameworks-benchmark'
    fixture = File.binread('test.bin')
    body = "--#{boundary}\r\n" \
           "Content-Disposition: form-data; name=\"file\"; filename=\"test.bin\"\r\n" \
           "Content-Type: application/octet-stream\r\n\r\n"
    body = body.b + fixture + "\r\n--#{boundary}--\r\n"

    response = post('/upload', body, "multipart/form-data; boundary=#{boundary}")
    expect(response.code).to eq('200')
    expect(JSON.parse(response.body)).to eq('filename' => 'test.bin', 'size' => 4096)
  end

  it 'deserializes JSON and returns an empty body' do
    body = File.binread('.tasks/fixtures/deserialization.json')
    response = post('/deserialization', body, 'application/json')
    expect(response.code).to eq('200')
    expect(response.body.to_s).to be_empty

    invalid = post('/deserialization', '{invalid', 'application/json')
    expect(invalid.code).to eq('400')
  end

  it 'serializes the fixed 100-object payload' do
    response = get('/serialization')
    users = JSON.parse(response.body)
    expect(response.code).to eq('200')
    expect(users.length).to eq(100)
    expect(users.first).to eq('id' => 1, 'name' => 'User 1')
    expect(users.last).to eq('id' => 100, 'name' => 'User 100')
  end

  it 'computes the fixed quote in integer cents' do
    body = File.binread('.tasks/fixtures/compute.json')
    response = post('/compute', body, 'application/json')
    expect(response.code).to eq('200')
    expect(JSON.parse(response.body)).to eq(
      'subtotalCents' => 28_266,
      'discountCents' => 1_270,
      'taxCents' => 3_905,
      'totalCents' => 30_901
    )
  end

  it 'rejects invalid user IDs and compute inputs' do
    expect(get('/user/not-a-number').code).to eq('400')
    expect(get('/user/-1').code).to eq('400')

    invalid_order = '{"items":[{"quantity":1,"discountBps":0,"taxBps":0}]}'
    expect(post('/compute', invalid_order, 'application/json').code).to eq('400')
  end

  it 'limits JSON bodies even when transfer encoding is chunked' do
    body = '{"data":"' + ('a' * 65_536) + '"}'
    expect(post('/deserialization', body, 'application/json').code).to eq('413')

    request = Net::HTTP::Post.new('/deserialization')
    request['Content-Type'] = 'application/json'
    request['Transfer-Encoding'] = 'chunked'
    request.body_stream = StringIO.new(body)
    expect(http.request(request).code).to eq('413')
  end

  it 'rejects oversized multipart uploads' do
    boundary = 'web-frameworks-benchmark'
    body = "--#{boundary}\r\n" \
           "Content-Disposition: form-data; name=\"file\"; filename=\"test.bin\"\r\n" \
           "Content-Type: application/octet-stream\r\n\r\n" \
           "#{'a' * 65_536}\r\n--#{boundary}--\r\n"

    response = post('/upload', body, "multipart/form-data; boundary=#{boundary}")
    expect(response.code).to eq('413')
  end
end
