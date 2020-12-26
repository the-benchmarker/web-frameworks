import mike

get "/":
  send "ok"

get "/user/{id}":
  send(id) 

post "/user":
  send "ok"

startServer(3000)
