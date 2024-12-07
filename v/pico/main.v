import picoev
import picohttpparser

// keep-alive header response
fn callback(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response) {
	if req.method == 'GET' {
		if req.path == '/' {
			res.write_string('HTTP/1.1 200 OK\r\n')
			res.header('Connection', 'keep-alive')
			res.header('Content-Type', 'text/plain')
			res.header('Content-Length', '0')

			// \r\n is required before body response
			res.write_string('\r\n')
		} else if req.path.starts_with('/user/') {
			content := req.path[6..]

			res.write_string('HTTP/1.1 200 OK\r\n')
			res.header('Connection', 'keep-alive')
			res.header('Content-Type', 'text/plain')
			res.header('Content-Length', content.len.str())

			// \r\n is required before body response
			res.write_string('\r\n')
			res.write_string(content)
		}
	}
	if req.method == 'POST' {
		if req.path == '/user' {
			res.write_string('HTTP/1.1 200 OK\r\n')
			res.header('Connection', 'keep-alive')
			res.header('Content-Type', 'text/plain')
			res.header('Content-Length', '0')

			//\r\n is required before body response
			res.write_string('\r\n')
		}
	}
	res.end()
}

fn main() {
	mut server := picoev.new(port: 3000, cb: callback)!

	server.serve()
}
