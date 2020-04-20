import Vapor

let app = Application()
defer { app.shutdown() }

let response = Response()

app.get { _ in
    response
}

app.get("user", ":userID") { req in
    req.parameters.get("userID") ?? ""
}

app.post("empty") { _ in
    return response
}

try app.run()
