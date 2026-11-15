# require "spectre"

# app = Spectre.new

# app.get "/" do |req, res|
#   res.send ""
# end

# app.group "/user" do |user|
#   user.post "/" do |req, res|
#     res.send ""
#   end

#   user.get "/:id" do |req, res|
#     res.send req.params["id"]
#   end
# end

# Fiber::ExecutionContext.default.resize(maximum: System.cpu_count)

# app.listen 3000, "0.0.0.0", true


require "spectre"

app = Spectre.new

app.get("/") { |req, res| res.send "" }
app.post("/user") { |req, res| res.send "" }
app.get("/user/:id") { |req, res| res.send req.params["id"] }

Fiber::ExecutionContext.default.resize(maximum: System.cpu_count)

app.listen 3000, "0.0.0.0", true