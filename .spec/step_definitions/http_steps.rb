Given('request bodies use {word}') do |transport|
  raise ArgumentError, "Unknown transport: #{transport}" unless %w[content-length chunked].include?(transport)

  @chunked = transport == 'chunked'
end

Given('the request has no Content-Type') do
  @content_type = nil
end

Given('the request Content-Type is {string}') do |type|
  @content_type = type.empty? ? nil : type
end

Given('the request header {string} is {string}') do |name, value|
  @headers[name] = value
end

Given('the request body is:') do |body|
  @body = body
end

Given('the request body is {string}') do |body|
  @body = body
end

Given('the JSON payload is {string}') do |name|
  @body = deserialization_payload(name)
  @content_type = 'application/json'
end

Given('the upload payload is {string}') do |name|
  @body, @content_type, @expected_upload = upload_payload(name)
end

When('I send {word} {string}') do |method, path|
  @response = send_request(method, path, body: @body, content_type: @content_type,
                                         headers: @headers, chunked: @chunked)
end

Then('the response status is {int}') do |status|
  expect_status(@response, status)
end

Then('the response status is 400 or 404') do
  expect([400, 404]).to include(@response.code.to_i)
end

Then('the response is successful') do
  expect(@response.code.to_i).to be_between(200, 299)
end

Then('the response body is empty') do
  expect(@response.body.to_s).to be_empty
end

Then('the response body is {string}') do |body|
  expect(@response.body.to_s).to eq(body)
end

Then('the JSON response is:') do |body|
  expect_json_response(@response, JSON.parse(body))
end

Then('the JSON response has only the ID {string}') do |id|
  expect_json_response(@response, { 'id' => id })
end

Then('the JSON response contains 100 objects with only seed {string}') do |seed|
  expect_json_response(@response, Array.new(100) { { 'seed' => seed } })
end

Then('the JSON response has count {int} and checksum {int}') do |count, checksum|
  expect_json_response(@response, { 'count' => count, 'checksum' => checksum })
end

Then('the JSON response describes every uploaded byte') do
  expect_json_response(@response, @expected_upload)
end
