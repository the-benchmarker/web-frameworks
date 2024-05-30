using Pkg
Pkg.activate(pwd())

using Oxygen
using HTTP

@get "/" function()
    return text("")
end

@get "/user/{id}" function(request::HTTP.Request, id::String)
    return text(id)
end

@post "/user" function ()
   return text("")
end

serveparallel(host = "0.0.0.0", port = 3000, docs = false, metrics = false, access_log = nothing)
