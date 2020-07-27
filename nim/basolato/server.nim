# framework
import basolato/routing
# controller
import app/controllers/benchmark_controller

settings:
  port = Port(3000)

routes:
  get "/": route(newBenchmarkController(request).index())
  get "/user/@id": route(newBenchmarkController(request).show(@"id"))
  post "/user": route(newBenchmarkController(request).store())
