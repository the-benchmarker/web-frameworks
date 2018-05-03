import Vapor

var services = Services.default()
services.register(MiddlewareConfig())

let router = EngineRouter.default()
router.get("ping") { req in
    return "pong" as StaticString
}
services.register(router, as: Router.self)

let app = try Application(config: .default(), environment: .detect(), services: services)
try app.run()
