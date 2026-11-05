require_relative '../spec_helper'

RSpec.describe 'serialization' do
  %w[abcdefgh aaaaaaaa zzzzzzzz azbycxdw].each do |seed|
    it "returns 100 objects containing only the requested seed #{seed}" do
      response = send_request('GET', "/serialization?seed=#{seed}")

      expect_json_response(response, Array.new(100) { { 'seed' => seed } })
    end
  end

  it 'returns the requested objects without requiring special request headers' do
    response = send_request('GET', '/serialization?seed=abcdefgh')

    expect_json_response(response, Array.new(100) { { 'seed' => 'abcdefgh' } })
  end

  ['%73eed=abcdefgh', 'seed=%61%62%63%64%65%66%67%68'].each do |query|
    it "accepts the valid seed #{query} when it is percent-encoded" do
      expect_json_response(send_request('GET', "/serialization?#{query}"), Array.new(100) { { 'seed' => 'abcdefgh' } })
    end
  end

  {
    'missing query' => nil, 'empty query' => '', 'missing value' => 'seed', 'empty value' => 'seed=',
    'short value' => 'seed=abcdefg', 'long value' => 'seed=abcdefghi',
    'digits' => 'seed=12345678', 'one digit' => 'seed=abcdefg1', 'uppercase' => 'seed=ABCDEFGH',
    'punctuation' => 'seed=abcd-fgh', 'space' => 'seed=abcd+fgh', 'Unicode' => 'seed=abcd%C3%A9fgh',
    'trailing newline' => 'seed=abcdefgh%0A', 'leading newline' => 'seed=%0Aabcdefgh',
    'array' => 'seed[]=abcdefgh', 'object' => 'seed[value]=abcdefgh',
    'encoded array' => 'seed%5B%5D=abcdefgh', 'wrong name' => 'Seed=abcdefgh',
    'repeated values' => 'seed=abcdefgh&seed=zzzzzzzz', 'identical repeated values' => 'seed=abcdefgh&seed=abcdefgh',
    'encoded repeated name' => 'seed=abcdefgh&%73eed=zzzzzzzz',
    'encoded name first' => '%73eed=abcdefgh&seed=zzzzzzzz',
    'scalar and array' => 'seed=abcdefgh&seed[]=zzzzzzzz',
    'extra parameter' => 'seed=abcdefgh&count=100', 'extra empty parameter' => 'seed=abcdefgh&other='
  }.each do |description, query|
    it "rejects a request with #{description}" do
      path = query.nil? ? '/serialization' : "/serialization?#{query}"

      expect_status(send_request('GET', path), 400)
    end
  end

  it 'uses the current seed on successive requests' do
    %w[abcdefgh zzzzzzzz azbycxdw abcdefgh].each do |seed|
      expect_json_response(send_request('GET', "/serialization?seed=#{seed}"), Array.new(100) { { 'seed' => seed } })
    end
  end

  it 'requires the seed in the query even when the body includes one' do
    response = send_request('GET', '/serialization', body: JSON.generate(seed: 'abcdefgh'),
                                                     content_type: 'application/json')

    expect_status(response, 400)
  end

  it 'uses the query seed when the body includes a different seed' do
    response = send_request('GET', '/serialization?seed=abcdefgh', body: JSON.generate(seed: 'zzzzzzzz'),
                                                                   content_type: 'application/json')

    expect_json_response(response, Array.new(100) { { 'seed' => 'abcdefgh' } })
  end
end
