import mofuw

routes:
  get "/":
    mofuwOK("")

  get "/user/{id}":
    mofuwOK(req.params("id"))

  post "/user":
    mofuwOK("")

mofuwRun(3000)
