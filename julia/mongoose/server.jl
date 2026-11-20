using Pkg
Pkg.activate(pwd())

using Mongoose

app = App(; workers = 0)

const EMPTY = Response(200, Pair{String, String}[], "")

get!(app, "/") do req
    EMPTY
end

get!(app, "/user/:id") do req, id
    Response(200, Pair{String, String}[], id)
end

post!(app, "/user") do req
    EMPTY
end

freeze!(app)
start!(app; host = "0.0.0.0", port = 3000)
