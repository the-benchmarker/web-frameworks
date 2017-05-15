import Vapor

let drop = Droplet(environment: .production)

drop.get("/") { _ in
    return ""
}

drop.get("user", String.self) { _, userID in
    return userID
}

drop.post("user") { _ in
    return ""
}

drop.run(servers: [
    "default": (host: "127.0.0.1", port: 3000, securityLayer: .none)
])
