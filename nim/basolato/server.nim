# framework
import basolato
# middleware
import app/http/middlewares/auth_middleware
import app/http/middlewares/set_headers_middleware
# controller
import app/http/controllers/benchmark_controller


let ROUTES = @[
  Route.group("", @[
    Route.get("/", benchmark_controller.index),
    Route.get("/user/{id:str}", benchmark_controller.show),
    Route.get("/user/{id:int}", benchmark_controller.show),
    Route.get("/user", benchmark_controller.user),

    # Route.group("/api", @[
    # ])
    # .middleware(set_headers_middleware.setCorsHeadersMiddleware),
  ])
  # .middleware(set_headers_middleware.setSecureHeadersMiddlware)
  # .middleware(auth_middleware.checkCsrfTokenMiddleware),
]

serve(ROUTES)
