require "spectre"

app = Spectre.new

app.get("/") { |req, res| res.send "" }
app.post("/user") { |req, res| res.send "" }
app.get("/user/:id") { |req, res| res.send req.params["id"] }

Fiber::ExecutionContext.default.resize(maximum: System.cpu_count)

app.listen 3000, "0.0.0.0", true