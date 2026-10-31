require_relative '../spec_helper'

RSpec.describe 'JSON serialization' do
  %w[abcdefgh zzzzzzzz].each do |seed|
    %w[identity gzip,br,zstd].each do |encoding|
      it "serializes 100 objects for seed #{seed} with Accept-Encoding: #{encoding}" do
        response = send_request('GET', "/serialization?seed=#{seed}", encoding: encoding)

        expect_json_response(response, Array.new(100) { { 'seed' => seed } })
      end
    end
  end

  [
    '', 'seed=', 'seed=abcdefg', 'seed=abcdefghi', 'seed=abcdefg1', 'seed=12345678',
    'seed=ABCDEFGH', 'seed=abcd-fgh', 'seed=abcdefgh%0A', 'seed=abcd%C3%A9fgh',
    'seed[]=abcdefgh', 'seed[value]=abcdefgh', 'seed=abcdefgh&seed=zzzzzzzz',
    'seed=abcdefgh&%73eed=zzzzzzzz', 'seed=abcdefgh&count=100', 'seed=abcdefgh&other=value'
  ].each do |query|
    it "rejects an invalid serialization query: #{query.inspect}" do
      expect_status(send_request('GET', "/serialization?#{query}", encoding: 'gzip, br, zstd'), 400)
    end
  end

  it 'does not use a conditional request to bypass serialization' do
    response = send_request('GET', '/serialization?seed=abcdefgh', headers: { 'If-None-Match' => '*' })

    expect_json_response(response, Array.new(100) { { 'seed' => 'abcdefgh' } })
  end

  it 'does not use a JSON body in place of the query seed' do
    response = send_request('GET', '/serialization', body: JSON.generate(seed: 'abcdefgh'),
                                                     content_type: 'application/json')

    expect_status(response, 400)
  end

  it 'uses the query seed when the JSON body contains another seed' do
    response = send_request('GET', '/serialization?seed=abcdefgh', body: JSON.generate(seed: 'zzzzzzzz'),
                                                                   content_type: 'application/json')

    expect_json_response(response, Array.new(100) { { 'seed' => 'abcdefgh' } })
  end
end
