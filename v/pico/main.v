import picoev
import picohttpparser
import os
import log

// BenchmarkServer represents the pico benchmark server
struct BenchmarkServer {
	port int
	server &picoev.Server
}

// new_benchmark_server creates a new pico benchmark server instance
fn new_benchmark_server(port int) BenchmarkServer {
	return BenchmarkServer{
		port: port
		server: picoev.new(port: port, cb: callback)!
	}
}

// callback handles incoming HTTP requests and routes them to appropriate controllers
// This is the request handler for picoev server
fn callback(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response) {
	// Log the request for debugging (skip health check to reduce overhead)
	if req.path != '/health' {
		log.debug('${req.method} ${req.path}')
	}

	if req.method == 'GET' {
		if req.path == '/' {
			log.debug('Root endpoint accessed')
			res.write_string('HTTP/1.1 200 OK\r\n')
			res.header('Connection', 'keep-alive')
			res.header('Content-Type', 'text/plain')
			res.header('Content-Length', '0')

			// \r\n is required before body response
			res.write_string('\r\n')
		} else if req.path.starts_with('/user/') {
			content := req.path[6..]
			log.debug('User endpoint accessed with ID: ${content}')

			res.write_string('HTTP/1.1 200 OK\r\n')
			res.header('Connection', 'keep-alive')
			res.header('Content-Type', 'text/plain')
			res.header('Content-Length', content.len.str())

			// \r\n is required before body response
			res.write_string('\r\n')
			res.write_string(content)
		} else if req.path == '/health' {
			// Health check endpoint
			res.write_string('HTTP/1.1 200 OK\r\n')
			res.header('Connection', 'keep-alive')
			res.header('Content-Type', 'text/plain')
			res.header('Content-Length', '2')
			res.write_string('\r\n')
			res.write_string('OK')
		}
	} else if req.method == 'POST' {
		if req.path == '/user' {
			log.debug('Create user endpoint accessed')
			res.write_string('HTTP/1.1 201 Created\r\n')
			res.header('Connection', 'keep-alive')
			res.header('Content-Type', 'text/plain')
			res.header('Content-Length', '0')

			// \r\n is required before body response
			res.write_string('\r\n')
		} else {
			// Method not allowed
			log.debug('Method not allowed: ${req.method} ${req.path}')
			res.write_string('HTTP/1.1 405 Method Not Allowed\r\n')
			res.header('Connection', 'keep-alive')
			res.header('Content-Type', 'text/plain')
			res.header('Content-Length', '19')
			res.write_string('\r\n')
			res.write_string('Method Not Allowed')
		}
	} else {
		// Method not allowed
		log.debug('Method not allowed: ${req.method} ${req.path}')
		res.write_string('HTTP/1.1 405 Method Not Allowed\r\n')
		res.header('Connection', 'keep-alive')
		res.header('Content-Type', 'text/plain')
		res.header('Content-Length', '19')
		res.write_string('\r\n')
		res.write_string('Method Not Allowed')
	}
	res.end()
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

	log.info('Starting pico benchmark server on port ${port}')

	// Create and run server
	mut server := new_benchmark_server(port)
	server.server.serve()
}
