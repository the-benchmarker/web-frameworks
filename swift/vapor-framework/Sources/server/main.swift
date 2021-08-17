import Vapor

var env = Environment(name: Environment.get("VAPOR_ENV") ?? "development")
try LoggingSystem.bootstrap(from: &env)
let app = Application()
defer { app.shutdown() }
app.middleware = .init()

app.logger.logLevel = .critical

app.get { _ in
    Response()
}

app.get("user", ":userID") { req in
    req.parameters.get("userID") ?? ""
}

app.post("user") { _ in
    Response()
}

app.post("empty") { _ in
    Response()
}

app.http.server.configuration.hostname = Environment.get("SERVER_HOSTNAME") ?? "0.0.0.0"
if let portString = Environment.get("SERVER_PORT"), let port = Int(portString) {
    app.http.server.configuration.port = port
} else {
    app.http.server.configuration.port = 3000
}

try app.run()
