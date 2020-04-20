import Vapor

let app = Application()
defer { app.shutdown() }

app.get { _ in
    Response()
}

app.get("user", ":userID") { req in
    req.parameters.get("userID") ?? ""
}

app.post("empty") { _ in
    Response()
}

try app.run()
