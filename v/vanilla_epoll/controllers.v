module main

import strings
import vanilla.http_server.http1_1.response

const http_ok_response = 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

const http_created_response = 'HTTP/1.1 201 Created\r\nContent-Type: text/plain\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

// home_controller handles the root endpoint for benchmarking
// Returns an empty response with 200 OK status
// 
// Args:
//   params: URL parameters (unused for root endpoint)
// 
// Returns:
//   HTTP response with empty body
fn home_controller(params []string) ![]u8 {
	defer {
		unsafe { params.free() }
	}
	return http_ok_response
}

// get_users_controller handles GET /users requests (unused in benchmark, but kept for compatibility)
// Returns an empty response with 200 OK status
// 
// Args:
//   params: URL parameters (unused)
// 
// Returns:
//   HTTP response with empty body
fn get_users_controller(params []string) ![]u8 {
	defer {
		unsafe { params.free() }
	}
	return http_ok_response
}

// get_user_controller handles GET /user/{id} requests
// Returns the user ID as plain text with 200 OK status
// 
// Args:
//   params: URL parameters containing the user ID
// 
// Returns:
//   HTTP response with user ID as body
@[direct_array_access; manualfree]
fn get_user_controller(params []string) ![]u8 {
	defer {
		unsafe {
			if params.len > 0 {
				params[0].free()
			}
			params.free()
		}
	}
	
	if params.len == 0 {
		return response.tiny_bad_request_response
	}
	id := params[0]
	response_body := id

	mut sb := strings.new_builder(200)
	sb.write_string('HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: ')
	sb.write_string(response_body.len.str())
	sb.write_string('\r\nConnection: keep-alive\r\n\r\n')
	sb.write_string(response_body)

	unsafe { response_body.free() }
	return sb
}

// create_user_controller handles POST /user requests
// Returns an empty response with 201 Created status
// 
// Args:
//   params: URL parameters (unused)
// 
// Returns:
//   HTTP response with empty body
fn create_user_controller(params []string) ![]u8 {
	defer {
		unsafe { params.free() }
	}
	return http_created_response
}

// health_check_controller handles GET /health requests
// Returns "OK" with 200 OK status for health monitoring
// 
// Args:
//   params: URL parameters (unused)
// 
// Returns:
//   HTTP response with "OK" body
fn health_check_controller(params []string) ![]u8 {
	defer {
		unsafe { params.free() }
	}
	mut sb := strings.new_builder(64)
	sb.write_string('HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 2\r\nConnection: keep-alive\r\n\r\n')
	sb.write_string('OK')
	return sb
}
