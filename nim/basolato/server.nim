# framework
import basolato
# controller
import app/controllers/benchmark_controller

var routes = newRoutes()

routes.get("/", benchmark_controller.index)
routes.get("/user/{id:int}", benchmark_controller.show)
routes.post("/user", benchmark_controller.store)

serve(routes, 3000)
