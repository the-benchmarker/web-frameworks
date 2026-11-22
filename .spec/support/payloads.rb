module ContractPayloads
  def deserialization_payload(name)
    items = Array.new(100) { |id| { 'id' => id } }
    invalid = {
      'empty' => '', 'whitespace' => " \r\n\t", 'truncated object' => '{',
      'truncated array' => '{"items":[', 'missing items' => '{}', 'null' => 'null',
      'array' => '[]', 'boolean' => 'true', 'number' => '42', 'string' => '"text"',
      'trailing garbage' => '{"items":[]} garbage', 'two objects' => '{}{}',
      'non-JSON keys' => '{items:[]}', 'trailing comma' => '{"items":[],}'
    }
    return invalid.fetch(name) if invalid.key?(name)

    case name
    when 'fixture' then return fixture_body
    when 'minimal' # IDs 0 through 99
    when 'top-level items' then return JSON.generate(items)
    when /\Aall IDs (.+)\z/
      # Preserve numeric notation, including integral floats and exponents.
      records = Array.new(100, "{\"id\":#{Regexp.last_match(1)}}").join(',')
      return "{\"items\":[#{records}]}"
    when 'extra fields'
      items.each { |item| item.merge!('name' => 'example', 'nested' => { 'id' => 1_000_000 }) }
      return JSON.generate(items: items, id: 1_000_000, metadata: { items: [1, 2, 3] })
    when /\Aload token (\d+)\z/
      token = Regexp.last_match(1).to_i
      index = -1
      body = fixture_body.gsub(/("id"\s*:\s*)(\d+)/) do
        index += 1
        "#{Regexp.last_match(1)}#{index < 5 ? token : 100_000 + Regexp.last_match(2).to_i}"
      end
      raise 'Unexpected load fixture size' unless body.bytesize == 10_742

      return body
    when /\Alast ID (\d+)\z/ then items.last['id'] = Regexp.last_match(1).to_i
    when /\Aitem count (\d+)\z/ then items = Array.new(Regexp.last_match(1).to_i) { |id| { 'id' => id } }
    when /\Aitems value (.+)\z/ then return json_body(JSON.parse(Regexp.last_match(1)))
    when /\Aitem value (.+)\z/ then items[50] = JSON.parse(Regexp.last_match(1))
    when /\AID (.+) at (\d+)\z/
      value, position = Regexp.last_match.captures
      items[position.to_i]['id'] = JSON.parse(value)
    else raise ArgumentError, "Unknown JSON payload: #{name}"
    end
    json_body(items)
  end

  def upload_payload(name)
    bytes = upload_bytes
    parts = upload_parts(bytes: bytes)
    boundary = 'benchmark-boundary'
    type = "multipart/form-data; boundary=#{boundary}"
    raw = nil

    case name
    when 'fixture' then bytes = upload_bytes
    when 'empty file' then bytes = ''.b
    when 'one byte' then bytes = "\xff".b
    when 'unsigned edge bytes' then bytes = "\x80\x00\xff".b
    when 'every byte value' then bytes = (0..255).to_a.pack('C*')
    when 'embedded newlines' then bytes = "\r\nhello\r\n"
    when 'boundary-like file data' then bytes = 'prefix--benchmark-boundary-suffix'
    when /\Afilename (.+)\z/
      bytes = 'different contents'
      parts = upload_parts(bytes: bytes, filename: Regexp.last_match(1), content_type: 'text/plain')
    when 'no file media type' then parts = upload_parts(content_type: nil)
    when /\Aboundary (.+)\z/
      boundary = Regexp.last_match(1)
      parameter = boundary.include?(':') ? "\"#{boundary}\"" : boundary
      type = "multipart/form-data; boundary=#{parameter}"
    when /\Aload token (\d+)\z/
      token = Regexp.last_match(1)
      bytes *= 64
      bytes[0, 30] = token * 5
    when 'middle byte changed'
      bytes.setbyte(bytes.bytesize / 2, bytes.getbyte(bytes.bytesize / 2) ^ 0xff)
    when 'text fields'
      parts = [{ name: 'description', body: 'before' }] + parts + [{ name: 'note', body: 'after' }]
    when 'unselected first' then parts = upload_parts(bytes: '', filename: '') + parts
    when 'unselected last' then parts += upload_parts(bytes: '', filename: '')
    when 'no file' then parts = []
    when 'wrong field' then parts = parts.map { |part| part.merge(name: 'other') }
    when 'text file field' then parts = [{ name: 'file', body: 'text' }]
    when 'repeated empty uploads' then parts = upload_parts(bytes: '') + upload_parts(bytes: '', filename: 'second.bin')
    when 'repeated uploads' then parts += upload_parts(bytes: 'second', filename: 'second.bin')
    when 'file then text' then parts += [{ name: 'file', body: 'text' }]
    when 'text then file' then parts = [{ name: 'file', body: 'text' }] + parts
    when 'one unselected' then parts = upload_parts(bytes: '', filename: '')
    when 'two unselected' then parts = upload_parts(bytes: '', filename: '') * 2
    when 'repeated with unselected' then parts += upload_parts(bytes: '', filename: '') + upload_parts(bytes: 'second')
    when 'empty body' then raw = ''
    when 'malformed' then raw = '{'
    when 'incomplete delimiter' then raw = "--#{boundary}\r\n"
    when 'mismatched boundary' then raw = "--wrong-boundary--\r\n"
    when 'truncated headers' then raw = "--#{boundary}\r\nContent-Disposition: form-data; name=\"file\""
    when 'incomplete trailing part'
      raw = multipart_body.sub("--#{boundary}--\r\n",
                               "--#{boundary}\r\nContent-Disposition: form-data; name=\"broken\"")
    when 'missing boundary' then type = 'multipart/form-data'
    when 'empty boundary' then type = 'multipart/form-data; boundary='
    when 'quoted empty boundary' then type = 'multipart/form-data; boundary=""'
    else raise ArgumentError, "Unknown upload payload: #{name}"
    end

    # Keep custom part metadata when changing file contents.
    parts.first[:body] = bytes if parts.length == 1 && parts.first[:filename] == 'payload.bin'
    [raw || multipart_body(parts, boundary: boundary), type, upload_summary(bytes: bytes)]
  end
end
