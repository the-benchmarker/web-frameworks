require_relative 'spec_helper'

RSpec.describe 'multipart uploads', :complete do
  let(:boundary) { 'benchmark-boundary' }
  let(:media_type) { "multipart/form-data; boundary=#{boundary}" }

  %w[identity gzip,br,zstd].each do |encoding|
    [false, true].each do |chunked|
      it "reads the uploaded file, chunked=#{chunked}, Accept-Encoding: #{encoding}" do
        response = send_request('POST', '/upload', body: multipart_body, content_type: media_type,
                                                   chunked: chunked, encoding: encoding)

        expect_json_response(response, upload_summary)
      end
    end
  end

  [
    { seed: 'zzzzzzzz', bytes: '', filename: 'empty.bin', content_type: 'application/octet-stream' },
    { seed: 'ijklmnop', bytes: "\x00\x80\xff".b, filename: 'bytes.bin', content_type: 'application/octet-stream' },
    { seed: 'qrstuvwx', bytes: 'different contents', filename: 'sample.txt', content_type: 'text/plain' }
  ].each do |attributes|
    it "uses the body metadata and bytes for #{attributes.fetch(:filename)}" do
      response = send_request('POST', '/upload', body: multipart_body(upload_parts(**attributes)),
                                                 content_type: media_type)

      expect_json_response(response, upload_summary(**attributes))
    end
  end

  it 'accepts a different multipart boundary' do
    response = send_request('POST', '/upload', body: multipart_body(boundary: 'another-boundary'),
                                               content_type: 'multipart/form-data; boundary=another-boundary')

    expect_json_response(response, upload_summary)
  end

  %w[abcdefg abcdefghi abcdefg1 12345678 ABCDEFGH].each do |seed|
    it "rejects invalid upload seed #{seed}" do
      response = send_request('POST', '/upload', body: multipart_body(upload_parts(seed: seed)),
                                                 content_type: media_type)

      expect_status(response, 400)
    end
  end

  %w[seed file].each do |name|
    it "rejects a missing #{name} part without substituting query values" do
      parts = upload_parts.reject { |part| part.fetch(:name) == name }
      response = send_request('POST', '/upload?seed=abcdefgh&file=payload.bin', body: multipart_body(parts),
                                                                                content_type: media_type)

      expect_status(response, 400)
    end

    it "rejects repeated #{name} parts" do
      parts = upload_parts
      parts << parts.find { |part| part.fetch(:name) == name }.dup
      response = send_request('POST', '/upload', body: multipart_body(parts), content_type: media_type)

      expect_status(response, 400)
    end
  end

  it 'rejects a text field in place of the uploaded file' do
    parts = [upload_parts.first, { name: 'file', body: 'not a file' }]
    response = send_request('POST', '/upload', body: multipart_body(parts), content_type: media_type)

    expect_status(response, 400)
  end

  ['', '--benchmark-boundary\r\n', '--wrong-boundary--\r\n'].each do |body|
    it "rejects malformed multipart data: #{body.inspect}" do
      response = send_request('POST', '/upload', body: body.gsub('\\r\\n', "\r\n"), content_type: media_type,
                                                 encoding: 'gzip, br, zstd')

      expect_status(response, 400)
    end
  end

  it 'rejects a multipart media type without a boundary' do
    response = send_request('POST', '/upload', body: multipart_body, content_type: 'multipart/form-data')

    expect_status(response, 400)
  end

  [nil, 'application/json', 'text/plain', 'application/x-www-form-urlencoded'].each do |type|
    it "rejects unsupported upload media type #{type.inspect}" do
      response = send_request('POST', '/upload', body: multipart_body, content_type: type)

      expect_status(response, 415)
    end
  end

  [false, true].each do |chunked|
    it "accepts a multipart body at the 64 KiB limit, chunked=#{chunked}" do
      bytes = 'x' * (HttpHelpers::BODY_LIMIT - multipart_body(upload_parts(bytes: '')).bytesize)
      response = send_request('POST', '/upload', body: multipart_body(upload_parts(bytes: bytes)),
                                                 content_type: media_type, chunked: chunked)

      expect_json_response(response, upload_summary(bytes: bytes))
    end

    it "rejects a multipart body above the 64 KiB limit, chunked=#{chunked}" do
      bytes = 'x' * (HttpHelpers::BODY_LIMIT + 1 - multipart_body(upload_parts(bytes: '')).bytesize)
      response = send_request('POST', '/upload', body: multipart_body(upload_parts(bytes: bytes)),
                                                 content_type: media_type, chunked: chunked, encoding: 'gzip, br, zstd')

      expect_status(response, 413)
    end
  end
end
