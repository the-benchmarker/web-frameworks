module main

import strings

fn home_controller() ![]u8 {
	response := 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n'.bytes()
	return response
}

fn get_user_controller(id string) ![]u8 {
	body := id.str()
	content_length := body.len
	mut sb := strings.new_builder(128)
	sb.write_string('HTTP/1.1 200 OK\r\n')
	sb.write_string('Content-Type: text/plain\r\n')
	sb.write_string('Content-Length: ')
	sb.write_string(content_length.str())
	sb.write_string('\r\n\r\n')
	sb.write_string(body)
	return sb
}

fn create_user_controller() ![]u8 {
	response := 'HTTP/1.1 201 Created\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n'.bytes()
	return response
}

fn not_found_response() ![]u8 {
	body := '404 Not Found'
	content_length := body.len
	mut sb := strings.new_builder(128)
	sb.write_string('HTTP/1.1 404 Not Found\r\n')
	sb.write_string('Content-Type: text/plain\r\n')

	sb.write_string('Content-Length: ')
	sb.write_string(content_length.str())
	sb.write_string('\r\n\r\n')
	sb.write_string(body)
	return sb
}
