module main

struct Slice {
	start int
	len   int
}

struct HttpRequest {
mut:
	buffer  []u8
	method  Slice
	path    Slice
	version Slice
	headers map[string]Slice
}

fn parse_request_line(mut req HttpRequest) ! {
	mut i := 0
	// Parse HTTP method
	for i < req.buffer.len && req.buffer[i] != ` ` {
		i++
	}
	req.method = Slice{
		start: 0
		len:   i
	}
	i++

	// Parse path
	mut path_start := i
	for i < req.buffer.len && req.buffer[i] != ` ` {
		i++
	}
	req.path = Slice{
		start: path_start
		len:   i - path_start
	}
	i++

	// Parse HTTP version
	mut version_start := i
	for i < req.buffer.len && req.buffer[i] != `\r` {
		i++
	}
	req.version = Slice{
		start: version_start
		len:   i - version_start
	}

	// Move to the end of the request line
	if i + 1 < req.buffer.len && req.buffer[i] == `\r` && req.buffer[i + 1] == `\n` {
		i += 2
	} else {
		return error('Invalid HTTP request line')
	}
}

fn parse_headers(mut req HttpRequest, offset &int) ! {
	for offset < req.buffer.len {
		// End of headers
		if req.buffer[*offset] == `\r` && req.buffer[*offset + 1] == `\n` {
			unsafe {
				*offset += 2
			}
			break
		}

		// Parse header name
		mut header_start := *offset
		for offset < req.buffer.len && req.buffer[*offset] != `:` {
			unsafe {
				*offset = *offset + 1
			}
		}
		header_name := req.buffer[header_start..*offset].bytestr()
		unsafe {
			*offset++ // Skip the colon
		}
		// Skip whitespace after the colon
		for offset < req.buffer.len && req.buffer[*offset] == ` ` {
			unsafe {
				*offset++
			}
		}

		// Parse header value
		mut value_start := *offset
		for offset < req.buffer.len && req.buffer[*offset] != `\r` {
			unsafe {
				*offset++
			}
		}
		header_value := Slice{
			start: value_start
			len:   *offset - value_start
		}
		req.headers[header_name] = header_value

		// Move to the next line
		if *offset + 1 < req.buffer.len && req.buffer[*offset] == `\r`
			&& req.buffer[*offset + 1] == `\n` {
			unsafe {
				*offset += 2
			}
		} else {
			return error('Invalid header line')
		}
	}
}

fn decode_http_request(buffer []u8) !HttpRequest {
	mut req := HttpRequest{
		buffer:  buffer
		headers: map[string]Slice{}
	}
	offset := 0

	parse_request_line(mut req)!
	parse_headers(mut req, &offset)!

	return req
}

// Helper function to convert Slice to string for debugging
fn slice_to_string(buffer []u8, s Slice) string {
	return buffer[s.start..s.start + s.len].bytestr()
}
