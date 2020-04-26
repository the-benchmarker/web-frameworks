require "./controllers/all"

module Server
  Shivneri.routes = [{
    controller: DefaultController,
    path:       "/*",
  }, {
    controller: UserController,
    path:       "/user",
  }]
end
