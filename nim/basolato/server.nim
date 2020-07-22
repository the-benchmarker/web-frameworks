# framework
import basolato/routing
# middleware
import app/middlewares/custom_headers_middleware
import app/middlewares/framework_middleware
# controller
import app/controllers/benchmark_controller

settings:
  port = Port(3000)

routes:
  # Framework
  error Http404: http404Route
  error Exception: exceptionRoute
  # before: framework

  get "/": route(newBenchmarkController(request).index())
  get "/user/@id": route(newBenchmarkController(request).show(@"id"))
  post "/user": route(newBenchmarkController(request).store())
