import picoev
import picohttpparser

fn callback(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response) {
	if req.method == 'GET' {
		if req.path == '/' {
			res.http_ok()
		} else if req.path.starts_with('/user/') {
			res.http_ok()
			id := req.path[6..]
			res.body(id)
		}
	}
	if req.method == 'POST' {
		if req.path == '/user' {
			res.http_ok()
		}
	}
	res.end()
}

fn main() {
	mut server := picoev.new(port: 3000, host: '0.0.0.0', cb: callback)

	server.serve()
}
