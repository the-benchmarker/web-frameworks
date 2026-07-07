module main

import fasthttp
import os
import log

// BenchmarkServer represents the fasthttp benchmark server
struct BenchmarkServer {
	port int
	server &fasthttp.Server
}

// new_benchmark_server creates a new benchmark server instance
fn new_benchmark_server(port int) BenchmarkServer {
	return BenchmarkServer{
		port: port
		server: fasthttp.new_server(fasthttp.ServerConfig{
			port:    port
			handler: handle_request
			// Optimize for benchmarking: disable features not needed
			max_request_body_size: 16 * 1024 * 1024 // 16 MB
		})!
	}
}

// handle_request handles incoming HTTP requests and routes them to appropriate controllers
fn handle_request(req fasthttp.HttpRequest) !fasthttp.HttpResponse {
	method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()

	// Log the request for debugging (skip health check to reduce overhead)
	if path != '/health' {
		log.debug('${method} ${path}')
	}

	try {
		if method == 'GET' {
			if path == '/' {
				log.debug('Root endpoint accessed')
				return fasthttp.HttpResponse{
					content: home_controller()!
				}
			} else if path.starts_with('/user/') {
				id := path[6..]
				log.debug('User endpoint accessed with ID: ${id}')
				return fasthttp.HttpResponse{
					content: get_user_controller(id)!
				}
			} else if path == '/health' {
				return fasthttp.HttpResponse{
					content: health_check_controller()!
				}
			}
		} else if method == 'POST' {
			if path == '/user' {
				log.debug('Create user endpoint accessed')
				return fasthttp.HttpResponse{
					content: create_user_controller()!
				}
			}
		}

		// Method not allowed or path not found
		log.debug('Request not found: ${method} ${path}')
		return fasthttp.HttpResponse{
			content: not_found_response()!
		}
	} catch {
		log.error('Error handling request: ${err}')
		return fasthttp.HttpResponse{
			content: internal_server_error_response()!
		}
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

	log.info('Starting fasthttp benchmark server on port ${port}')

	// Create and run server
	mut server := new_benchmark_server(port)
	server.server.run() or { 
		log.error('Failed to start server: ${err}')
	}
}
