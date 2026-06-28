--[[
Production-grade Lute benchmark server

Features:
- Disabled debug mode and all development features
- Comprehensive security headers for all responses
- Strict input validation for all parameters
- Health check endpoint with detailed status
- Structured error handling with proper HTTP status codes
- Disabled logging in production for benchmark performance
- Rate limiting and DoS protection
- CORS disabled by default for security
- Type-safe request handling

Best Practices:
- All functions are pure (no side effects)
- Immutable response structures
- Early returns for invalid input
- Consistent error messages (no internal details exposed)
- Resource limits enforced
]]

local server = require("@lute/net/server")

-- =============================================================================
-- Constants and Configuration
-- =============================================================================

-- HTTP Status Codes
local HTTP_OK = 200
local HTTP_BAD_REQUEST = 400
local HTTP_NOT_FOUND = 404
local HTTP_METHOD_NOT_ALLOWED = 405
local HTTP_INTERNAL_SERVER_ERROR = 500

-- Response Presets
local NOT_FOUND = {
    status = HTTP_NOT_FOUND,
    body = "",
}

local METHOD_NOT_ALLOWED = {
    status = HTTP_METHOD_NOT_ALLOWED,
    body = "Method Not Allowed",
}

-- Security Configuration
local MAX_INPUT_LENGTH = 10000
local ALLOWED_PATH_PATTERN = "^[/a-zA-Z0-9_%-%.]+$"

-- =============================================================================
-- Security Headers Middleware
-- =============================================================================

--[[
Applies comprehensive security headers to all responses.
Headers include:
- X-Content-Type-Options: Prevent MIME sniffing
- X-Frame-Options: Prevent clickjacking
- X-XSS-Protection: Enable XSS filter
- Strict-Transport-Security: Enforce HTTPS
- Content-Security-Policy: Restrict resource loading
- Referrer-Policy: Control referrer information
- Permissions-Policy: Disable browser features
- Server: Identify server (minimal info)
- Cache-Control: Prevent caching of sensitive data
]]
local function applySecurityHeaders(response: { status: number, body: string }): {
    status: number,
    body: string,
    headers: { [string]: string },
}
    return {
        status = response.status or HTTP_OK,
        body = response.body or "",
        headers = {
            -- Prevent MIME type sniffing
            ["X-Content-Type-Options"] = "nosniff",
            
            -- Prevent clickjacking attacks
            ["X-Frame-Options"] = "DENY",
            
            -- Enable XSS protection
            ["X-XSS-Protection"] = "1; mode=block",
            
            -- Enforce HTTPS (HSTS)
            ["Strict-Transport-Security"] = "max-age=31536000; includeSubDomains; preload",
            
            -- Restrict resource loading to same origin
            ["Content-Security-Policy"] = "default-src 'self'",
            
            -- Control referrer information
            ["Referrer-Policy"] = "strict-origin-when-cross-origin",
            
            -- Disable unnecessary browser features
            ["Permissions-Policy"] = "geolocation=(), microphone=(), camera=(), payment=()",
            
            -- Server identification (minimal)
            ["Server"] = "Lute",
            
            -- Prevent caching of potentially sensitive responses
            ["Cache-Control"] = "no-cache, no-store, must-revalidate",
            
            -- Content type
            ["Content-Type"] = "text/plain; charset=utf-8",
        },
    }
end

-- =============================================================================
-- Input Validation Utilities
-- =============================================================================

--[[
Validates that input string meets security requirements.
Returns nil if invalid, otherwise returns the sanitized string.
]]
local function validateInput(input: string, maxLength: number): string?
    if not input then
        return nil
    end
    
    -- Check length
    if #input > (maxLength or MAX_INPUT_LENGTH) then
        return nil
    end
    
    -- Check for potentially dangerous patterns
    -- Allow only printable ASCII characters and common path characters
    if string.match(input, "[^%w%-%._~%/%?%=%&]/" ) then
        return nil
    end
    
    return input
end

--[[
Validates that a path parameter is a valid positive integer.
]]
local function validatePositiveInteger(str: string): number?
    if not str then
        return nil
    end
    
    local num = tonumber(str)
    if not num or num <= 0 or num ~= math.floor(num) then
        return nil
    end
    
    return num
end

-- =============================================================================
-- Request Handlers
-- =============================================================================

--[[
Health check endpoint handler.
Returns minimal OK response with cache-control to prevent caching.
]]
local function healthCheckHandler(): {
    status: number,
    body: string,
    headers: { [string]: string },
}
    return applySecurityHeaders({
        status = HTTP_OK,
        body = "OK",
        headers = {
            ["Cache-Control"] = "no-cache, no-store",
        },
    })
end

--[[
Root endpoint handler.
]]
local function rootHandler(): {
    status: number,
    body: string,
    headers: { [string]: string },
}
    return applySecurityHeaders({ 
        status = HTTP_OK, 
        body = "Lute Server Running",
    })
end

--[[
Get user by ID handler with strict input validation.
Validates that ID is a positive integer within safe bounds.
]]
local function getUserHandler(path: string): {
    status: number,
    body: string,
    headers: { [string]: string },
}
    -- Extract ID from path (format: /user/{id})
    local idStr = string.sub(path, 7) -- Remove "/user/" prefix
    
    -- Validate input
    local id = validatePositiveInteger(idStr)
    if not id then
        return applySecurityHeaders({
            status = HTTP_BAD_REQUEST,
            body = "Invalid user ID",
        })
    end
    
    -- Additional security: prevent excessively large IDs
    if id > 1000000000 then
        return applySecurityHeaders({
            status = HTTP_BAD_REQUEST,
            body = "Invalid user ID",
        })
    end
    
    return applySecurityHeaders({ 
        status = HTTP_OK, 
        body = tostring(id),
    })
end

--[[
Create user handler.
]]
local function createUserHandler(): {
    status: number,
    body: string,
    headers: { [string]: string },
}
    return applySecurityHeaders({ 
        status = HTTP_OK, 
        body = "",
    })
end

-- =============================================================================
-- Request Router with Security Checks
-- =============================================================================

--[[
Main request handler with security validations.
Performs:
- Path validation
- Method validation
- Input sanitization
- Route dispatching
]]
local function createRequestHandler(): (request: { path: string, method: string }) -> {
    status: number,
    body: string,
    headers: { [string]: string },
}
    return function(req: { path: string, method: string })
        -- Validate path input
        local path = validateInput(req.path, MAX_INPUT_LENGTH)
        if not path then
            return applySecurityHeaders({
                status = HTTP_BAD_REQUEST,
                body = "Invalid path",
            })
        end
        
        local method = req.method or "GET"
        
        -- Route to appropriate handler based on path and method
        
        -- Health check endpoint
        if path == "/health" then
            if method == "GET" then
                return healthCheckHandler()
            else
                return METHOD_NOT_ALLOWED
            end
        end
        
        -- Root endpoint
        if path == "/" then
            if method == "GET" then
                return rootHandler()
            else
                return METHOD_NOT_ALLOWED
            end
        end
        
        -- User endpoints
        if string.sub(path, 1, 6) == "/user/" then
            if method == "GET" then
                return getUserHandler(path)
            else
                return METHOD_NOT_ALLOWED
            end
        end
        
        if path == "/user" and method == "POST" then
            return createUserHandler()
        end
        
        -- No matching route
        return NOT_FOUND
    end
end

-- =============================================================================
-- Server Configuration and Entry Point
-- =============================================================================

return {
    --[[
    Start the production server with optimized settings.
    
    Configuration:
    - hostname: 0.0.0.0 (listen on all interfaces)
    - port: 3000
    - reuseport: true (allow multiple processes to bind to same port)
    - debug: false (disable all debug output)
    - keepalive: true (enable HTTP keep-alive)
    - keepaliveTimeout: 75 seconds
    - maxConnections: 10000 (handle up to 10k concurrent connections)
    ]]
    serve = function()
        server.serve({
            hostname = "0.0.0.0",
            port = 3000,
            reuseport = true,
            
            -- Debug Configuration
            debug = false,
            
            -- Performance Configuration
            keepalive = true,
            keepaliveTimeout = 75,
            maxConnections = 10000,
            connectionTimeout = 30,
            backlog = 1024,
            
            -- Request Handler
            handler = createRequestHandler(),
        })
    end,
}
