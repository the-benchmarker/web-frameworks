import Vapor

var services = Services.default()
services.register(MiddlewareConfig())

let router = EngineRouter.default()
let empty = HTTPResponse()

router.get("/") { _ in
    return empty
}
router.get("user", Int.parameter) { req -> String in
    let id = try req.parameters.next(Int.self)
    return "\(id)"
}
router.post("user") { _ in
    return empty
}

var middlewares = MiddlewareConfig() // Create _empty_ middleware config
middlewares.use(ErrorMiddleware.self) // Catches errors and converts to HTTP response
services.register(middlewares)
services.register(router, as: Router.self)

let app = try Application(config: .default(), environment: .detect(), services: services)
try app.run()
