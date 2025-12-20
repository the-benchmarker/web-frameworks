using Pkg
Pkg.activate(pwd())

using Mongoose

server = SyncServer()

function getroot(request::Request, params::Dict{String,String})
    return Response(200, "Content-Type: text/plain\r\n", "")
end

function getuserid(request::Request, params::Dict{String,String})
    return Response(200, "Content-Type: text/plain\r\n", params["id"])
end

function postuser(request::Request, params::Dict{String,String})
    return Response(200, "Content-Type: text/plain\r\n", "")
end

route!(server, :get, "/", getroot)
route!(server, :get, "/user/:id", getuserid)
route!(server, :post, "/user", postuser)

start!(server, host = "0.0.0.0", port = 3000)
