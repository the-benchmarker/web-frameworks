# framework
import basolato
# controller
import app/http/controllers/benchmark_controller


let ROUTES = @[
  Route.get("/", benchmark_controller.index),
  Route.get("/user/{id:str}", benchmark_controller.show),
  Route.get("/user/{id:int}", benchmark_controller.show),
  Route.post("/user", benchmark_controller.store),
]

serve(ROUTES)
