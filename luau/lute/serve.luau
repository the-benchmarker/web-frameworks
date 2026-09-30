local server = require("@lute/net/server")

local NOT_FOUND = { status = 404, body = "" }

return {
	serve = function()
		server.serve({
			hostname = "0.0.0.0",
			port = 3000,
			reuseport = true,
			handler = function(req)
				local path = req.path

				if path == "/" then
					return ""
				end

				local method = req.method

				if method == "GET" and string.sub(path, 1, 6) == "/user/" then
					return string.sub(path, 7)
				end

				if method == "POST" and path == "/user" then
					return ""
				end

				return NOT_FOUND
			end,
		})
	end,
}
