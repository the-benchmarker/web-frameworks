import Hummingbird

@main
struct Server {
    static func main() async throws {
        let env = Environment()
        let serverHostName = env.get("SERVER_HOSTNAME") ?? "127.0.0.1"
        let serverPort = env.get("SERVER_PORT", as: Int.self) ?? 8080

        let router = Router()
        router.get("/") { _, _ in
            HTTPResponse.Status.ok
        }

        router.get("user/:id") { _, ctx -> String in
            ctx.parameters.get("id") ?? ""
        }

        router.post("user") { _, _ in
            HTTPResponse.Status.ok
        }

        let app = Application(
            router: router,
            configuration: .init(address: .hostname(serverHostName, port: serverPort))
        )

        try await app.run()
    }
}
