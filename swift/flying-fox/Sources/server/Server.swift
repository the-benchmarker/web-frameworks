import FlyingFox

@main
struct Server {
    static func main() async throws {
        let server = HTTPServer(port: 3000)

        await server.appendRoute("GET /") { _ in
            return HTTPResponse(statusCode: .ok)
        }

        await server.appendRoute("GET /user/:id") { request in
            let user = request.routeParameters["id"] ?? ""
            return HTTPResponse(statusCode: .ok, body: user.data(using: .utf8)!)
        }

        await server.appendRoute("POST /user") { _ in
            return HTTPResponse(statusCode: .ok)
        }

        try await server.start()
    }
}
