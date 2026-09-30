using Pkg
Pkg.activate(pwd())

using HTTP

R = HTTP.Router()
HTTP.register!(R, "GET", "/", (req::HTTP.Request) -> HTTP.Response(200, ""))
HTTP.register!(R, "GET", "/user/{id}", (req::HTTP.Request) -> HTTP.Response(200, HTTP.getparams(req)["id"]))
HTTP.register!(R, "POST", "/user", (req::HTTP.Request) -> HTTP.Response(200, ""))
HTTP.serve(R, "0.0.0.0", 3000)
