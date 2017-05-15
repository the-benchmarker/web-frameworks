import Vapor

/// Manually override config
let config = Config([:])
config.arguments = ["vapor", "serve", "--port=3000"]
config.environment = .production

let drop = try Droplet(config: config, middleware: [])

drop.get("/") { req in
    return ""
}

drop.get("user", String.parameter) { req in
    let userID = try req.parameters.next(String.self)
    return userID
}

drop.post("user") { req in
    return ""
}

try drop.run()
