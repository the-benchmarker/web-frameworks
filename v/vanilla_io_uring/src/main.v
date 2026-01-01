module main

import vanilla.http_server
import vanilla.http_server.http1_1.response
import vanilla.http_server.http1_1.request_parser

fn handle_request(req_buffer []u8, client_conn_fd int) ![]u8 {
	req := request_parser.decode_http_request(req_buffer)!

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

	return response.tiny_bad_request_response
}

fn main() {
	mut server := http_server.new_server(http_server.ServerConfig{
		port:            3000
		request_handler: handle_request
		io_multiplexing: unsafe { http_server.IOBackend.io_uring }
	})!

	server.run()
}
