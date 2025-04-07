module main

// handle_request finds and executes the handler for a given route.
// It takes an HttpRequest object as an argument and returns the response as a byte array.
fn handle_request(req_buffer []u8, client_conn_fd int) ![]u8 {
	req := decode_http_request(req_buffer)!

	method := unsafe { tos(&req.buffer[req.method.start], req.method.len) }
	path := unsafe { tos(&req.buffer[req.path.start], req.path.len) }

	if method == 'GET' {
		if path == '/' {
			return home_controller([])
		} else if path.starts_with('/user/') {
			id := path[6..]
			return get_user_controller([id])
		}
	} else if method == 'POST' {
		if path == '/user' {
			return create_user_controller([])
		}
	}

	return tiny_bad_request_response
}

fn main() {
	mut vanilla := Server{
		request_handler: handle_request
		port:            3000
	}

	vanilla.run()
}
