module main

import strings

// home_controller handles the root endpoint for benchmarking
// Returns an empty response with 200 OK status
fn home_controller() ![]u8 {
	response := 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n'.bytes()
	return response
}

// get_user_controller handles GET /user/{id} requests
// Returns the user ID as plain text with 200 OK status
// 
// Args:
//   id: User identifier
// 
// Returns:
//   HTTP response with user ID as body
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

// create_user_controller handles POST /user requests
// Returns an empty response with 201 Created status
fn create_user_controller() ![]u8 {
	response := 'HTTP/1.1 201 Created\r\nContent-Type: text/plain\r\nContent-Length: 0\r\n\r\n'.bytes()
	return response
}

// health_check_controller handles GET /health requests
// Returns "OK" with 200 OK status for health monitoring
fn health_check_controller() ![]u8 {
	body := 'OK'
	mut sb := strings.new_builder(64)
	sb.write_string('HTTP/1.1 200 OK\r\n')
	sb.write_string('Content-Type: text/plain\r\n')
	sb.write_string('Content-Length: 2\r\n\r\n')
	sb.write_string(body)
	return sb
}

// not_found_response handles 404 Not Found errors
// Returns a 404 response with error message
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

// internal_server_error_response handles 500 Internal Server Error
// Returns a 500 response with error message
fn internal_server_error_response() ![]u8 {
	body := 'Internal Server Error'
	content_length := body.len
	mut sb := strings.new_builder(128)
	sb.write_string('HTTP/1.1 500 Internal Server Error\r\n')
	sb.write_string('Content-Type: text/plain\r\n')
	sb.write_string('Content-Length: ')
	sb.write_string(content_length.str())
	sb.write_string('\r\n\r\n')
	sb.write_string(body)
	return sb
}
