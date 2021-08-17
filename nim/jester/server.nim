import jester

settings:
  port = Port(8080)

routes:
  get "/":
    resp ""

  get "/user/@id":
    resp @"id"

  post "/user":
    resp ""
