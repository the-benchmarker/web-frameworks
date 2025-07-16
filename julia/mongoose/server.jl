using Pkg
Pkg.activate(pwd())

using Mongoose

function getroot(conn; kwargs...)
    return mg_text_reply(conn, 200, "")
end

function getuserid(conn; kwargs...)
    return mg_text_reply(conn, 200, kwargs[:params][:id])
end

function postuser(conn; kwargs...)
    return mg_text_reply(conn, 200, "")
end

mg_register!("get", "/", getroot)
mg_register!("get", "/user/:id", getuserid)
mg_register!("post", "/user", postuser)

mg_serve!(host = "0.0.0.0", port = 3000, async = false)
# mg_shutdown!()
