import FlyingFox

let server = HTTPServer(port: 3000, logger: .disabled)

await server.appendRoute("GET /") { _ in
    return HTTPResponse(statusCode: .ok)
}

await server.appendRoute("GET /user/:id") { (user: String) -> HTTPResponse in
    return HTTPResponse(statusCode: .ok, body: user.data(using: .utf8)!)
}

await server.appendRoute("POST /user") { _ in
    return HTTPResponse(statusCode: .ok)
}

try await server.run()
