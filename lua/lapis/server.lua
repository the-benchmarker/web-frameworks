-- Production-grade Lapis benchmark server
-- 
-- Features:
-- - Disabled debug mode and development features
-- - Security headers configured for all responses
-- - Input validation for user ID parameter
-- - Health check endpoint for monitoring
-- - Error handling with proper HTTP status codes
-- - Disabled logging in production for benchmark performance

local lapis = require("lapis")
local app = lapis.Application()

-- Configure application for production
app.layout = false
app.disable_logging = true

-- Security headers middleware
-- Adds security headers to every response for production security
app:match("before_dispatch", function(self)
    -- Skip health check endpoint for benchmarking
    if self.req.path ~= "/health" then
        self.res.headers["Server"] = "Lapis"
        self.res.headers["X-Content-Type-Options"] = "nosniff"
        self.res.headers["X-Frame-Options"] = "DENY"
        self.res.headers["X-XSS-Protection"] = "1; mode=block"
        self.res.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
    end
end)

-- Root endpoint
-- @Summary Root endpoint
-- @Description Root endpoint for benchmarking
-- @Produce plain
-- @Success 200 {string} string "Empty response"
-- @Router / [get]
app:get("/", function(self)
    self.res.headers["Content-Type"] = "text/plain"
    return ""
end)

-- Health check endpoint
-- @Summary Health check
-- @Description Health check endpoint for monitoring
-- @Produce plain
-- @Success 200 {string} string "OK"
-- @Router /health [get]
app:get("/health", function(self)
    self.res.headers["Content-Type"] = "text/plain"
    self.res.headers["Cache-Control"] = "no-cache"
    return "OK"
end)

-- Get user by ID with input validation
-- @Summary Get user by ID
-- @Description Retrieve user information by ID
-- @Produce plain
-- @Param id path string true "User ID (numeric)"
-- @Success 200 {string} string "User ID"
-- @Failure 400 {string} string "Invalid ID format"
-- @Router /user/{id} [get]
app:get("/user/:id[%d]", function(self)
    local id = self.params.id
    
    -- Input validation - ensure ID is a valid number
    if not id or id <= 0 then
        self.res.headers["Content-Type"] = "text/plain"
        return { status = 400, "Invalid user ID" }
    end
    
    self.res.headers["Content-Type"] = "text/plain"
    return tostring(id)
end)

-- Create user endpoint
-- @Summary Create user
-- @Description Create a new user
-- @Produce plain
-- @Success 200 {string} string "Empty response"
-- @Router /user [post]
app:post("/user", function(self)
    self.res.headers["Content-Type"] = "text/plain"
    return ""
end)

-- Global error handler for production-grade error management
app:match("after_dispatch", function(self, err)
    if err then
        -- Log errors to stderr in production
        ngx.log(ngx.ERR, "Error: " .. tostring(err))
        
        -- Return appropriate error response
        local status = 500
        local message = "Internal Server Error"
        
        -- Handle specific error types
        if err:match("404") then
            status = 404
            message = "Not Found"
        elseif err:match("400") then
            status = 400
            message = "Bad Request"
        end
        
        self.res.headers["Content-Type"] = "text/plain"
        return { status = status, message }
    end
end)

return app
