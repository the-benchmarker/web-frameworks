using Genie
import Genie.Router: route, POST, @params

route("/user/:id") do
    return @params(:id)
end

route("/user", method = POST) do
    return ""
end

route("/") do
    return ""
end

Genie.AppServer.startup(3000, "0.0.0.0")

Base.JLOptions().isinteractive==0 && wait()
