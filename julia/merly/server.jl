using Pkg
Pkg.activate(pwd())

using Merly

@page "/" HTTP.Response(200, "")

@page "/user/:id" HTTP.Response(200, string(request.params["id"]))

@route POST "/user" begin
    HTTP.Response(200, "")
end

start(host = "0.0.0.0", port = 3000, verbose = true)
