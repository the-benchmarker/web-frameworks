using Pkg
Pkg.activate(pwd())

using Genie, Genie.Router

Genie.config.app_env = "prod"
Genie.config.log_requests = false
Genie.config.server_handler_mode = :threads

route("/") do
    ""
end

route("/user/:id") do
    params(:id)
end

route("/user", method = POST) do
    ""
end

up(3000, "0.0.0.0", async = false)
