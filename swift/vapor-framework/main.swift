import Vapor

let app = Application()
defer { app.shutdown() }
app.middleware = .init()

let empty = Response()

app.get { _ in
    empty
}

app.get("user", ":userID") { req in
    req.parameters.get("userID") ?? ""
}

app.post("user") { _ in
    empty
}

app.http.server.configuration.hostname = "0.0.0.0"
app.http.server.configuration.port = 3000

try app.run()
