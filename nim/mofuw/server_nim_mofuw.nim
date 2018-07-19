import mofuw

routes:
  get "/":
    mofuwOK("")

  get "/user/{id}":
    mofuwOK(req.params("id"))

  post "/user":
    mofuwOK("")

newServeCtx(
  port = 3000,
  handler = mofuwHandler
).serve()
