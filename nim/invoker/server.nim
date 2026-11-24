import invoker
# import routes

proc setup*(app: App) =
  app.get("/") do (req: Request, res: Response):
    res.send("")

  app.post("/user") do (req: Request, res: Response):
    res.send("")

  app.get("/user/:id") do (req: Request, res: Response):
    res.send(req.params["id"])

var cfg = defaultConfig()
cfg.host = "0.0.0.0"
cfg.port = 3000
cfg.reusePort = true     # the `true` argument to listen
cfg.threads = 0          # 0 = one worker per CPU core
serve(setup, cfg)