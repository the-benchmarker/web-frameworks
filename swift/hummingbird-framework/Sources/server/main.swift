import Hummingbird

func runApp() throws {
    let env = HBEnvironment()
    let serverHostName = env.get("SERVER_HOSTNAME") ?? "127.0.0.1"
    let serverPort = env.get("SERVER_PORT", as: Int.self) ?? 8080
    let app = HBApplication(
        configuration: .init(
            address: .hostname(serverHostName, port: serverPort),
            enableHttpPipelining: false
        )
    )
    
    app.router.get("/") { _ in
        return HBResponse(status: .ok)
    }
    
    app.router.get("user/:id") { req -> String in
        req.parameters.get("id") ?? ""
    }

    app.router.post("user") { _  in
        return HBResponse(status: .ok)
    }
    
    try app.start()
    app.wait()
}

try runApp()
