local turbo = require "turbo"

turbo.log["categories"] = {
    ["success"] = false,
    ["notice"] = false,
    ["warning"] = false,
    ["error"] = false,
    ["debug"] = false,
    ["development"] = false,
}

local RootHandler = class("RootHandler", turbo.web.RequestHandler)
function RootHandler:get()
    self:write("")
end

local GetUserHandler = class("GetUserHandler", turbo.web.RequestHandler)
function GetUserHandler:get(id)
    self:write(id)
end

local CreateUserHandler = class("CreateUserHandler", turbo.web.RequestHandler)
function CreateUserHandler:post()
    self:write("")
end

turbo.web.Application({
    {"^/$", RootHandler},
    {"^/user/(%d+)", GetUserHandler},
    {"^/user", CreateUserHandler},
}):listen(3000, "0.0.0.0")

turbo.ioloop.instance():start()