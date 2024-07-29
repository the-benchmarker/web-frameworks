local config = require("lapis.config")

config("production", {
  server = "nginx",
  bind_host = "0.0.0.0",
  port = 3000,
  code_cache = "on",
  num_workers = "auto",
  logging = false,
})