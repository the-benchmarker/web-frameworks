require_relative '../spec_helper'

RSpec.describe 'POST /upload' do
  [false, true].each do |chunked|
    context "with #{chunked ? 'HTTP/1.1 chunked transfer' : 'Content-Length'}" do
      let(:chunked) { chunked }
      let(:media_type) { 'multipart/form-data; boundary=benchmark-boundary' }

      def upload(parts = upload_parts, body: multipart_body(parts), type: media_type, path: '/upload', **)
        send_request('POST', path, body: body, content_type: type, chunked: chunked, **)
      end

      it 'returns only the unsigned edge bytes and SHA-256 of the shared file' do
        expect_json_response(upload, {
                               'first_byte' => 97, 'last_byte' => 100,
                               'checksum' => '1f91053dcf43206eb082c0962785d35d86d4f629345f8bff25be7394416db908'
                             })
      end

      it 'also serves clients requesting identity encoding' do
        expect_json_response(upload, upload_summary)
      end

      {
        'empty file' => ''.b,
        'one byte' => "\xff".b,
        'unsigned edge bytes' => "\x80\x00\xff".b,
        'every byte value' => (0..255).to_a.pack('C*'),
        'embedded newlines' => "\r\nhello\r\n",
        'boundary-like file data' => 'prefix--benchmark-boundary-suffix'
      }.each do |description, bytes|
        it "hashes and samples #{description}" do
          expect_json_response(upload(upload_parts(bytes: bytes)), upload_summary(bytes: bytes))
        end
      end

      ['sample.txt', 'another-file.dat'].each do |filename|
        it "accepts filename #{filename} and a different file media type" do
          parts = upload_parts(bytes: 'different contents', filename: filename, content_type: 'text/plain')

          expect_json_response(upload(parts), upload_summary(bytes: 'different contents'))
        end
      end

      it 'accepts an uploaded file without a part Content-Type' do
        expect_json_response(upload(upload_parts(content_type: nil)), upload_summary)
      end

      {
        'another-boundary' => 'another-boundary',
        'quoted:boundary' => '"quoted:boundary"',
        'x' => 'x'
      }.each do |boundary, parameter|
        it "accepts valid boundary #{parameter}" do
          response = upload(body: multipart_body(boundary: boundary),
                            type: "multipart/form-data; boundary=#{parameter}")

          expect_json_response(response, upload_summary)
        end
      end

      it 'accepts the 256 KiB workload with changing 30-byte tokens' do
        %w[123456 987654].each do |token|
          bytes = upload_bytes * 64
          bytes[0, 30] = token * 5

          expect_json_response(upload(upload_parts(bytes: bytes)), upload_summary(bytes: bytes))
        end
      end

      it 'hashes every byte even when the size and edge bytes stay the same' do
        original = upload_bytes
        changed = original.dup
        midpoint = changed.bytesize / 2
        changed.setbyte(midpoint, changed.getbyte(midpoint) ^ 0xff)

        [original, changed, original].each do |bytes|
          expect_json_response(upload(upload_parts(bytes: bytes)), upload_summary(bytes: bytes))
        end
      end

      it 'allows unrelated text fields around the uploaded file' do
        parts = [{ name: 'description', body: 'before' }] + upload_parts + [{ name: 'note', body: 'after' }]

        expect_json_response(upload(parts), upload_summary)
      end

      it 'rejects an empty form' do
        expect_status(upload([]), 400)
      end

      it 'does not substitute a query parameter for the uploaded file' do
        expect_status(upload([], path: '/upload?file=payload.bin'), 400)
      end

      it 'rejects a file under another part name' do
        expect_status(upload(upload_parts.map { |part| part.merge(name: 'other') }), 400)
      end

      it 'rejects a text field named file' do
        expect_status(upload([{ name: 'file', body: 'not an uploaded file' }]), 400)
      end

      it 'rejects two uploaded file parts even when the files are empty' do
        expect_status(upload(upload_parts(bytes: '') + upload_parts(bytes: '', filename: 'second.bin')), 400)
      end

      it 'rejects two different uploaded file parts' do
        expect_status(upload(upload_parts + upload_parts(bytes: 'second', filename: 'second.bin')), 400)
      end

      [true, false].each do |file_first|
        it "rejects a file and a text field with the same name, file first: #{file_first}" do
          parts = upload_parts + [{ name: 'file', body: 'text' }]
          parts.reverse! unless file_first

          expect_status(upload(parts), 400)
        end

        it "accepts an unselected input alongside a file, file first: #{file_first}" do
          parts = upload_parts + upload_parts(bytes: '', filename: '')
          parts.reverse! unless file_first

          expect_json_response(upload(parts), upload_summary)
        end
      end

      [1, 2].each do |count|
        it "rejects #{count} unselected inputs when no uploaded file remains" do
          expect_status(upload(upload_parts(bytes: '', filename: '') * count), 400)
        end
      end

      it 'still rejects repeated files when an unselected input is present' do
        parts = upload_parts + upload_parts(bytes: '', filename: '') + upload_parts(bytes: 'second')

        expect_status(upload(parts), 400)
      end

      {
        'empty body' => '',
        'incomplete opening delimiter' => "--benchmark-boundary\r\n",
        'mismatched boundary' => "--wrong-boundary--\r\n",
        'truncated part headers' => "--benchmark-boundary\r\nContent-Disposition: form-data; name=\"file\""
      }.each do |description, body|
        it "rejects malformed multipart data: #{description}" do
          expect_status(upload(body: body), 400)
        end
      end

      it 'rejects an incomplete trailing part after a valid uploaded file' do
        body = multipart_body.sub("--benchmark-boundary--\r\n",
                                  "--benchmark-boundary\r\nContent-Disposition: form-data; name=\"broken\"")

        expect_status(upload(body: body), 400)
      end

      ['multipart/form-data', 'multipart/form-data; boundary=', 'multipart/form-data; boundary=""'].each do |type|
        it "rejects a missing or empty boundary in #{type}" do
          expect_status(upload(type: type), 400)
        end
      end

      [nil, 'application/json', 'text/plain', 'application/x-www-form-urlencoded', 'multipart/mixed'].each do |type|
        it "returns 415 for unsupported media type #{type.inspect}" do
          expect_status(upload(type: type, headers: { 'Accept' => 'application/json' }), 415)
        end

        it "checks #{type.inspect} before parsing malformed multipart input" do
          expect_status(upload(body: '{', type: type), 415)
        end

        it "returns 415 for an empty body with #{type.inspect}" do
          expect_status(upload(body: '', type: type), 415)
        end
      end

      it 'can process a valid upload after a parsing failure and a validation failure' do
        expect_status(upload(body: '{'), 400)
        expect_status(upload(upload_parts * 2), 400)
        expect_json_response(upload, upload_summary)
      end
    end
  end
end
