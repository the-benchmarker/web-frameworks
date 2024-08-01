import caprese

config:
  sslLib = None
  headerServer = true
  headerDate = true
  headerContentType = true
  connectionPreferred = InternalConnection
  postRequestMethod = true

server(ip = "0.0.0.0", port = 3000):
  routes:
    get "/": return "".addActiveHeader("text").send
    get startsWith("/user/"): return reqUrl[6..^1].addHeader("text").send
    post "/user": return "".addActiveHeader("text").send

serverStart()
