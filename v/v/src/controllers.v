module main

const h_response_body = '{"message": "Hello, world!"}'
const http_ok_response = 'HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

const http_created_response = 'HTTP/1.1 201 Created\r\nContent-Type: application/json\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

fn home_controller(params map[string]string) ![]u8 {
	return http_ok_response
}

fn get_users_controller(params map[string]string) ![]u8 {
	return http_ok_response
}

@[manualfree]
fn get_user_controller(params map[string]string) ![]u8 {
	id := params['id'] or { return error('User ID required') }
	response_body := id

	response := 'HTTP/1.1 200 OK\r
Content-Type: text/plain\r
Content-Length: ${response_body.len}\r
Connection: keep-alive\r
\r
${response_body}'.bytes()

	defer {
		unsafe {
			id.free()
			response_body.free()
			response.free()
		}
	}

	return response
}

fn create_user_controller(params map[string]string) ![]u8 {
	return http_ok_response
}
