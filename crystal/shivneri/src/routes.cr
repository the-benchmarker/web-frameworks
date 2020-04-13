require "./controllers/all"

module App
  Shivneri.routes = [{
    controller: DefaultController,
    path:       "/*",
  }, {
    controller: UserController,
    path:       "/user",
  }]
end
