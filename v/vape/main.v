import exastencil.vape

server := vape.Server{port: 3000}

server.mount('GET', '/', fn (req vape.Request) vape.Response {
	return vape.Response{
		body: ''
	}
})

server.mount('GET', '/user/:id', fn (req vape.Request) vape.Response {
	id := req.query['id']
	return vape.Response{
		body: '$id'
	}
})

server.mount('POST', '/user', fn (req vape.Request) vape.Response {
	return vape.Response{
		body: ''
	}
})

server.serve()
