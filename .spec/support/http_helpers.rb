require 'digest'
require 'json'
require 'net/http'
require 'stringio'
require 'uri'

module HttpHelpers
  ROOT = File.expand_path('../..', __dir__)

  def http
    if ENV['BENCHMARK_URL']
      uri = URI(ENV.fetch('BENCHMARK_URL'))
      unless uri.scheme == 'http' && uri.host && ['', '/'].include?(uri.path) && !uri.query && !uri.fragment
        raise ArgumentError, 'BENCHMARK_URL must be an HTTP origin'
      end

      host = uri.hostname
      port = uri.port
    else
      directory = File.join(ROOT, ENV.fetch('LANGUAGE'), ENV.fetch('FRAMEWORK'))
      host = File.read(File.join(directory, "ip-#{ENV.fetch('ENGINE')}.txt")).strip
      port = 3000
    end

    Net::HTTP.new(host, port, nil).tap do |client|
      client.open_timeout = 3
      client.read_timeout = 5
    end
  end

  def send_request(method, path, body: nil, content_type: nil, headers: {}, chunked: false, encoding: 'identity')
    klass = Net::HTTP.const_get(method.capitalize)
    request = klass.new(path, { 'Accept-Encoding' => encoding }.merge(headers))
    request['Content-Type'] = content_type if content_type

    if chunked
      request['Transfer-Encoding'] = 'chunked'
      request.body_stream = StringIO.new(body.b)
    elsif body
      request.body = body
    end

    http.request(request).tap do |response|
      # Explicit Accept-Encoding disables Net::HTTP's automatic decompression.
      expect(response['content-encoding']).to be_nil
    end
  end

  def expect_status(response, status)
    expect(response.code.to_i).to eq(status)
  end

  def expect_empty_response(response)
    expect_status(response, 200)
    expect(response.body.to_s).to be_empty
  end

  def expect_json_response(response, expected)
    expect_status(response, 200)
    expect(response.content_type).to eq('application/json')
    expect([nil, 'utf-8']).to include(response.type_params['charset']&.downcase)
    expect(JSON.parse(response.body)).to eq(expected)
  end

  def fixture_body
    File.binread(File.join(ROOT, 'deserialization.json'))
  end

  def upload_bytes
    File.binread(File.join(ROOT, 'upload.bin'))
  end

  def fixture_items
    JSON.parse(fixture_body).fetch('items')
  end

  def json_body(items = fixture_items)
    JSON.generate(items: items)
  end

  def deserialization_summary(items)
    { 'count' => items.length, 'checksum' => items.sum { |item| item.fetch('id').to_i } }
  end

  def upload_parts(bytes: upload_bytes, filename: 'payload.bin', content_type: 'application/octet-stream')
    [{ name: 'file', body: bytes, filename: filename, content_type: content_type }]
  end

  def multipart_body(parts = upload_parts, boundary: 'benchmark-boundary')
    parts.map do |part|
      disposition = "Content-Disposition: form-data; name=\"#{part.fetch(:name)}\""
      disposition += "; filename=\"#{part.fetch(:filename)}\"" if part.key?(:filename)
      headers = [disposition]
      headers << "Content-Type: #{part[:content_type]}" if part[:content_type]
      "--#{boundary}\r\n#{headers.join("\r\n")}\r\n\r\n".b + part.fetch(:body).b + "\r\n".b
    end.join.b + "--#{boundary}--\r\n".b
  end

  def upload_summary(bytes: upload_bytes)
    {
      'first_byte' => bytes.getbyte(0), 'last_byte' => bytes.getbyte(-1),
      'checksum' => Digest::SHA256.hexdigest(bytes)
    }
  end
end
