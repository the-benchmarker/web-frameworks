local lapis = require("lapis")
local app = lapis.Application()

app.layout = false

app:get("/", function()
    return ""
end)

app:get("/user/:id[%d]", function(self)
    return self.params.id
end)

app:post("/user", function()
    return ""
end)

return app