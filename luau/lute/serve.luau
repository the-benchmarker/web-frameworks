-- Production-grade Lute benchmark server
-- 
-- Features:
-- - Disabled debug mode and development features
-- - Security headers configured for all responses
-- - Input validation for user ID parameter
-- - Health check endpoint for monitoring
-- - Error handling with proper HTTP status codes
-- - Disabled logging in production for benchmark performance

local server = require("@lute/net/server")

local NOT_FOUND = { status = 404, body = "" }

-- Security headers for production
local function addSecurityHeaders(response)
    return {
        status = response.status or 200,
        body = response.body or "",
        headers = {
            ["Server"] = "Lute",
            ["X-Content-Type-Options"] = "nosniff",
            ["X-Frame-Options"] = "DENY",
            ["X-XSS-Protection"] = "1; mode=block",
            ["Cache-Control"] = "no-cache, no-store, must-revalidate",
            ["Content-Type"] = "text/plain",
        },
    }
end

-- Health check endpoint
local function healthCheckHandler()
    return addSecurityHeaders({
        status = 200,
        body = "OK",
        headers = {
            ["Cache-Control"] = "no-cache",
        },
    })
end

-- Root endpoint handler
local function rootHandler()
    return addSecurityHeaders({ status = 200, body = "" })
end

-- Get user by ID handler with input validation
local function getUserHandler(path)
    local id = string.sub(path, 7)
    
    -- Input validation - ensure ID is a valid number
    if not id or tonumber(id) == nil or tonumber(id) <= 0 then
        return addSecurityHeaders({
            status = 400,
            body = "Invalid user ID",
        })
    end
    
    return addSecurityHeaders({ status = 200, body = id })
end

-- Create user handler
local function createUserHandler()
    return addSecurityHeaders({ status = 200, body = "" })
end

return {
	serve = function()
		server.serve({
			hostname = "0.0.0.0",
			port = 3000,
			reuseport = true,
			-- Disable debug in production
			debug = false,
			-- Production settings for performance
			keepalive = true,
			keepaliveTimeout = 75,
			maxConnections = 1000,
			handler = function(req)
				local path = req.path
				local method = req.method

				-- Health check endpoint
				if path == "/health" then
					return healthCheckHandler()
				end

				-- Root endpoint
				if path == "/" then
					return rootHandler()
				end

				-- User endpoints
				if method == "GET" and string.sub(path, 1, 6) == "/user/" then
					return getUserHandler(path)
				end

				if method == "POST" and path == "/user" then
					return createUserHandler()
				end

				return NOT_FOUND
			end,
		})
	end,
}
