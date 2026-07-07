module main

import vanilla.http_server
import vanilla.http_server.http1_1.response
import vanilla.http_server.http1_1.request_parser
import os
import log

// BenchmarkServer represents the vanilla epoll benchmark server
struct BenchmarkServer {
	port int
	server &http_server.Server
}

// new_benchmark_server creates a new vanilla epoll benchmark server instance
fn new_benchmark_server(port int) BenchmarkServer {
	return BenchmarkServer{
		port: port
		server: http_server.new_server(http_server.ServerConfig{
			port:            port
			request_handler: handle_request
			io_multiplexing: unsafe { http_server.IOBackend.epoll }
		})!
	}
}

// handle_request handles incoming HTTP requests and routes them to appropriate controllers
fn handle_request(req_buffer []u8, client_conn_fd int, mut out []u8) ! {
	req := request_parser.decode_http_request(req_buffer)!

	method := unsafe { tos(&req.buffer[req.method.start], req.method.len) }
	path := unsafe { tos(&req.buffer[req.path.start], req.path.len) }

	// Log the request for debugging (skip health check to reduce overhead)
	if path != '/health' {
		log.debug('${method} ${path}')
	}

	try {
		if method == 'GET' {
			if path == '/' {
				log.debug('Root endpoint accessed')
				out << home_controller([])!
				return
			} else if path.starts_with('/user/') {
				id := path[6..]
				log.debug('User endpoint accessed with ID: ${id}')
				out << get_user_controller([id])!
				return
			} else if path == '/health' {
				// Health check endpoint
				out << health_check_controller([])!
				return
			}
		} else if method == 'POST' {
			if path == '/user' {
				log.debug('Create user endpoint accessed')
				out << create_user_controller([])!
				return
			}
		}

		// Method not allowed or path not found
		log.debug('Request not found: ${method} ${path}')
		out << response.tiny_not_found_response
	} catch {
		log.error('Error handling request: ${err}')
		out << response.tiny_internal_server_error_response
	}
}

fn main() {
	// Configure logging
	log.set_format(log.Format{
		timestamp: true
		level:     true
		message:   true
	})
	log.set_level(.debug)

	// Get port from environment or use default
	port := int(os.getenv('PORT') or { '3000' })

	log.info('Starting vanilla epoll benchmark server on port ${port}')

	// Create and run server
	mut server := new_benchmark_server(port)
	server.server.run()
}
