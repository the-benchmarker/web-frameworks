require_relative 'spec_helper'

RSpec.describe HttpHelpers do
  # Capture Net::HTTP's encoded request without needing a listening server.
  # Checking request headers before serialization would miss defaults added later.
  def wire_request(**)
    request = build_request('POST', '/example', **)
    socket = StringIO.new(''.b)
    def socket.continue_timeout
      nil
    end
    request.exec(socket, '1.1', request.path)
    socket.string.split("\r\n\r\n", 2)
  end

  it 'sends binary bodies with their byte length' do
    headers, body = wire_request(body: "\x00\x80\xff".b)

    expect(headers.split("\r\n")).to include('Content-Length: 3')
    expect(headers).not_to include('Transfer-Encoding:')
    expect(body).to eq("\x00\x80\xff".b)
  end

  it 'sends chunked bodies without Content-Length' do
    headers, body = wire_request(body: "\x00\x80\xff".b, chunked: true)

    expect(headers).to include('Transfer-Encoding: chunked')
    expect(headers).not_to include('Content-Length:')
    expect(body).to eq("3\r\n\x00\x80\xff\r\n0\r\n\r\n".b)
  end

  [false, true].each do |chunked|
    it "actually omits Content-Type when testing a missing media type, chunked: #{chunked}" do
      headers, = wire_request(body: '{}', chunked: chunked)

      expect(headers).not_to match(/content-type:/i)
    end
  end

  it 'keeps the supplied media type on the wire' do
    headers, = wire_request(body: '{}', content_type: 'application/problem+json; charset=utf-8')

    expect(headers).to include('Content-Type: application/problem+json; charset=utf-8')
  end

  it 'disables automatic response decompression while advertising compression' do
    request = build_request('GET', '/')

    expect(request['Accept-Encoding']).to eq('gzip, deflate, br, zstd')
    expect(request.decode_content).to be(false)
  end
end
