using Pkg
Pkg.activate(pwd())

using Mongoose

server = Server()

function getroot(request::Request)
    return Response("")
end

function getuserid(request::Request, id::String)
    return Response(id)
end

function postuser(request::Request)
    return Response("")
end

route!(server, :get, "/", getroot)
route!(server, :get, "/user/:id", getuserid)
route!(server, :post, "/user", postuser)

start!(server, host = "0.0.0.0", port = 3000)
