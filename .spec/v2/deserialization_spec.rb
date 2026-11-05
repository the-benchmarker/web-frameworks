require_relative '../spec_helper'

RSpec.describe 'POST /deserialization' do
  [false, true].each do |chunked|
    context "with #{chunked ? 'HTTP/1.1 chunked transfer' : 'Content-Length'}" do
      let(:items) { Array.new(100) { |id| { 'id' => id } } }
      let(:chunked) { chunked }

      def deserialize(body = json_body(items), type: 'application/json', path: '/deserialization', **)
        send_request('POST', path, body: body, content_type: type, chunked: chunked, **)
      end

      it 'returns only count and checksum for the shared fixture' do
        expect_json_response(deserialize(fixture_body), { 'count' => 100, 'checksum' => 4950 })
      end

      it 'accepts items containing only an ID, with identity encoding' do
        expect_json_response(deserialize, { 'count' => 100, 'checksum' => 4950 })
      end

      [0, 1_000_000, 2.0].each do |id|
        it "accepts 100 repeated IDs equal to #{id.inspect}" do
          items.each { |item| item['id'] = id }

          expect_json_response(deserialize, { 'count' => 100, 'checksum' => (100 * id).to_i })
        end
      end

      it 'accepts numeric integral exponent notation' do
        records = Array.new(100, '{"id":2e0}').join(',')
        body = "{\"items\":[#{records}]}"

        expect_json_response(deserialize(body), { 'count' => 100, 'checksum' => 200 })
      end

      it 'accepts additional object and item fields without counting their IDs' do
        items.each { |item| item.merge!('name' => 'example', 'nested' => { 'id' => 1_000_000 }) }
        body = JSON.generate(items: items, id: 1_000_000, metadata: { items: [1, 2, 3] })

        expect_json_response(deserialize(body), { 'count' => 100, 'checksum' => 4950 })
      end

      it 'computes a new checksum for successive bodies at the same URL' do
        [999_999, 0, 42].each do |id|
          items.last['id'] = id

          expect_json_response(deserialize, { 'count' => 100, 'checksum' => 4851 + id })
        end
      end

      it 'accepts the full 10,742-byte workload with changing six-digit IDs' do
        [123_456, 987_654].each do |token|
          index = -1
          body = fixture_body.gsub(/("id"\s*:\s*)(\d+)/) do
            index += 1
            value = index < 5 ? token : 100_000 + Regexp.last_match(2).to_i
            "#{Regexp.last_match(1)}#{value}"
          end

          expect(body.bytesize).to eq(10_742)
          expect_json_response(deserialize(body), { 'count' => 100, 'checksum' => 9_504_940 + (5 * token) })
        end
      end

      ['application/json', 'application/json; charset=utf-8', 'application/problem+json',
       'application/vnd.api+json', 'application/vnd.benchmark+json; charset=utf-8',
       'text/x-json', 'application/jsonrequest'].each do |type|
        it "accepts #{type}" do
          expect_json_response(deserialize(type: type), { 'count' => 100, 'checksum' => 4950 })
        end

        it "rejects malformed JSON with supported media type #{type}" do
          expect_status(deserialize('{', type: type), 400)
        end
      end

      [0, 1, 99, 101, 200].each do |count|
        it "rejects an array containing #{count} items" do
          expect_status(deserialize(json_body(Array.new(count) { |id| { 'id' => id } })), 400)
        end
      end

      [nil, false, true, 100, {}, 'items'].each do |value|
        it "rejects non-array items #{value.inspect}" do
          expect_status(deserialize(json_body(value)), 400)
        end
      end

      [nil, false, true, [], 'item', 7, {}].each do |value|
        it "rejects invalid item #{value.inspect}" do
          items[50] = value

          expect_status(deserialize, 400)
        end
      end

      [nil, false, true, '2', 1.5, -1, 1_000_001, [], {}].each do |id|
        [0, 50, 99].each do |position|
          it "rejects ID #{id.inspect} at item #{position}" do
            items[position]['id'] = id

            expect_status(deserialize, 400)
          end
        end
      end

      {
        'empty body' => '', 'whitespace' => " \r\n\t", 'truncated object' => '{',
        'truncated array' => '{"items":[', 'missing items' => '{}', 'null' => 'null',
        'array' => '[]', 'boolean' => 'true', 'number' => '42', 'string' => '"text"',
        'trailing garbage' => '{"items":[]} garbage', 'two objects' => '{}{}',
        'non-JSON keys' => '{items:[]}', 'trailing comma' => '{"items":[],}'
      }.each do |description, body|
        it "rejects #{description}" do
          expect_status(deserialize(body), 400)
        end
      end

      it 'rejects 100 items in a top-level array without the object wrapper' do
        expect_status(deserialize(JSON.generate(items)), 400)
      end

      %w[seed=abcdefgh items=100 other=value format=json %73eed=abcdefgh other= other].each do |query|
        it "rejects query parameters #{query}" do
          expect_status(deserialize(path: "/deserialization?#{query}"), 400)
        end
      end

      it 'does not substitute query items for a missing body field' do
        expect_status(deserialize('{}', path: '/deserialization?items[0][id]=1'), 400)
      end

      [nil, 'text/plain', 'application/xml', 'application/x-www-form-urlencoded', 'application/jsonp'].each do |type|
        it "returns 415 for #{type.inspect} even when Accept requests JSON" do
          expect_status(deserialize(type: type, headers: { 'Accept' => 'application/json' }), 415)
        end

        it "checks #{type.inspect} before attempting to parse malformed JSON" do
          expect_status(deserialize('{', type: type), 415)
        end

        it "returns 415 for an empty body with #{type.inspect}" do
          expect_status(deserialize('', type: type), 415)
        end
      end

      it 'can process a valid request after a validation error' do
        expect_status(deserialize('{}'), 400)
        expect_json_response(deserialize, { 'count' => 100, 'checksum' => 4950 })
      end
    end
  end
end
