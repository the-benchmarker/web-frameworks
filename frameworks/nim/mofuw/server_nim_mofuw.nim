import mofuw

proc handler(req: mofuwReq, res: mofuwRes) {.async.} =
  routes:
    get "/":
      mofuwResp(
        HTTP200,
        "text/plain",
        ""
      )

    get "/user/@id":
      mofuwResp(
        HTTP200,
        "text/plain",
        req.params["id"]
      )

    post "/user":
      mofuwResp(
        HTTP200,
        "text/plain",
        ""
      )

handler.mofuwRun(port = 3000, bufSize = 512)