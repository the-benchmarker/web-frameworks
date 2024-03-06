using Pkg
Pkg.activate(pwd())

using Oxygen
using HTTP

@get "/" function()
    ""
end

@get "/user/{id}" function(request::HTTP.Request, id)
    id
end

@post "/user" function ()
   ""
end

# Could be faster setting access_log = nothing and using multithreading with serveparallel
serve(host = "0.0.0.0", port = 3000, docs = false, metrics = false)
