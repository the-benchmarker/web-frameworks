import mofuw

routes:
  get "/":
    mofuwOK("")

  get "/user/{id}":
    mofuwOK(ctx.params("id"))

  post "/user":
    mofuwOK("")

newServeCtx(
  port = 3000,
  handler = mofuwHandler
).serve()
