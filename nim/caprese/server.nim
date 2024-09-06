import caprese

config:
  sslLib = None
  headerServer = true
  headerDate = true
  headerContentType = true
  activeHeader = true
  connectionPreferred = InternalConnection
  postRequestMethod = true

server(ip = "0.0.0.0", port = 3000):
  routes:
    get "/": "".addHeader("text").send
    get "/user/:id": id.addHeader("text").send
    post "/user": "".addHeader("text").send

serverStart()
