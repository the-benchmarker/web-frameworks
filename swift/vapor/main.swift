import Vapor

var services = Services.default()
services.register(MiddlewareConfig())

let router = EngineRouter.default()
let empty = HTTPResponse()
router.get("/") { req in
    return empty
}
router.get("user", String.parameter) { req in
    return try req.parameters.next(String.self)
}
router.post("user") { req in
    return empty
}
services.register(router, as: Router.self)

let app = try Application(config: .default(), environment: .detect(), services: services)
try app.run()
