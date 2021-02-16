import mike

get "/":
  send ""

get "/user/{id}":
  send(id) 

post "/user":
  send ""

startServer(3000)
