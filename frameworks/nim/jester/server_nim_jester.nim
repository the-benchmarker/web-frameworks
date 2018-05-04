import jester, asyncdispatch

settings:
    port = Port(3000)

routes:
  get "/":
    resp ""
    
  get "/user/@id":
    resp @"id"

  post "/user":
    resp ""

runForever()

