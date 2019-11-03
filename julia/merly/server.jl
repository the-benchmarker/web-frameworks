using Pkg
Pkg.activate(pwd())

using Merly

server = Merly.app()

@page "/" ""

@page "/user/:id>" "{{id}}"

@route POST "/user" begin
    res.body = ""
end

server.start(config = Dict("host" => "0.0.0.0", "port" => 3000), verbose = true)