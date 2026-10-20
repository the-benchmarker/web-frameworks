require_relative 'spec_helper'

RSpec.describe 'JSON deserialization', :complete do
  let(:items) { fixture_items }

  %w[identity gzip,br,zstd].each do |encoding|
    [false, true].each do |chunked|
      it "decodes 100 items, chunked=#{chunked}, Accept-Encoding: #{encoding}" do
        response = send_request('POST', '/deserialization', body: json_body(items),
                                                            content_type: 'application/json', chunked: chunked,
                                                            encoding: encoding)

        expect_json_response(response, deserialization_summary(items))
      end
    end
  end

  it 'uses the current body when the same URL receives different IDs' do
    [100, 1_000_000].each do |id|
      items.last['id'] = id
      response = send_request('POST', '/deserialization', body: json_body(items), content_type: 'application/json')

      expect_json_response(response, deserialization_summary(items))
    end
  end

  it 'accepts integral floating-point IDs without requiring a seed' do
    items.each { |item| item['id'] = item.fetch('id').to_f }
    response = send_request('POST', '/deserialization', body: json_body(items), content_type: 'application/json')

    expect_json_response(response, deserialization_summary(items))
  end

  it 'accepts a JSON charset parameter' do
    response = send_request('POST', '/deserialization', body: json_body(items),
                                                        content_type: 'application/json; charset=utf-8')

    expect_json_response(response, deserialization_summary(items))
  end

  [0, 1, 99, 101].each do |count|
    it "rejects #{count} items" do
      records = Array.new(count) { |id| { 'id' => id } }
      response = send_request('POST', '/deserialization', body: json_body(records), content_type: 'application/json')

      expect_status(response, 400)
    end
  end

  [nil, false, {}, 'items'].each do |value|
    it "rejects a non-array items value: #{value.inspect}" do
      response = send_request('POST', '/deserialization', body: json_body(value), content_type: 'application/json')

      expect_status(response, 400)
    end
  end

  [nil, false, 'item', 7, {}].each do |value|
    it "rejects an invalid item: #{value.inspect}" do
      items[50] = value
      response = send_request('POST', '/deserialization', body: json_body(items), content_type: 'application/json')

      expect_status(response, 400)
    end
  end

  [nil, false, true, '2', 1.5, -1, 1_000_001, [], {}].each do |id|
    it "rejects an invalid ID: #{id.inspect}" do
      items[50]['id'] = id
      response = send_request('POST', '/deserialization', body: json_body(items), content_type: 'application/json')

      expect_status(response, 400)
    end
  end

  ['', '{', '{"items":[', '{}', 'null', '[]'].each do |body|
    it "rejects malformed JSON or missing items: #{body.inspect}" do
      response = send_request('POST', '/deserialization', body: body, content_type: 'application/json',
                                                          encoding: 'gzip, br, zstd')

      expect_status(response, 400)
    end
  end

  %w[seed=abcdefgh items=100 other=value format=json].each do |query|
    it "rejects query parameters: #{query}" do
      response = send_request('POST', "/deserialization?#{query}", body: json_body(items),
                                                                   content_type: 'application/json')

      expect_status(response, 400)
    end
  end

  [nil, 'text/plain', 'application/xml', 'application/x-www-form-urlencoded'].each do |type|
    it "rejects unsupported media type #{type.inspect} even when JSON is requested" do
      response = send_request('POST', '/deserialization', body: json_body(items), content_type: type,
                                                          headers: { 'Accept' => 'application/json' })

      expect_status(response, 415)
    end
  end

  [false, true].each do |chunked|
    it "accepts a body at the 64 KiB limit, chunked=#{chunked}" do
      padding = 'x' * (HttpHelpers::BODY_LIMIT - json_body(items, padding: '').bytesize)
      response = send_request('POST', '/deserialization', body: json_body(items, padding: padding),
                                                          content_type: 'application/json', chunked: chunked)

      expect_json_response(response, deserialization_summary(items))
    end

    it "rejects a body above the 64 KiB limit, chunked=#{chunked}" do
      padding = 'x' * (HttpHelpers::BODY_LIMIT + 1 - json_body(items, padding: '').bytesize)
      response = send_request('POST', '/deserialization', body: json_body(items, padding: padding),
                                                          content_type: 'application/json', chunked: chunked,
                                                          encoding: 'gzip, br, zstd')

      expect_status(response, 413)
    end
  end
end
