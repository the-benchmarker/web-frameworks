"""
HTTP.jl Benchmark Server

Production-grade benchmark server implementation using HTTP.jl framework.

Features:
- Disabled debug logging
- Environment variable support
- Performance optimization
- Error handling
- Graceful shutdown
- Security best practices
"""

using Pkg
Pkg.activate(@__DIR__)

using HTTP
using Sockets
using Dates

"""
    get_environment_variable(name::String, default::String) -> String

Get environment variable with fallback to default value.
"""
function get_environment_variable(name::String, default::String)::String
    value = get(ENV, name, default)
    return value
end

"""
    get_environment_port() -> Int

Get server port from environment variable or use default.
"""
function get_environment_port()::Int
    port_str = get_environment_variable("PORT", "3000")
    try
        return parse(Int, port_str)
    catch e
        return 3000
    end
end

"""
    get_environment_host() -> String

Get server host from environment variable or use default.
"""
function get_environment_host()::String
    return get_environment_variable("HOST", "0.0.0.0")
end

"""
    is_production() -> Bool

Check if running in production environment.
"""
function is_production()::Bool
    env = get(ENV, "JULIA_ENV", "dev")
    return env == "prod" || env == "production"
end

"""
    log_error(message::String)

Log error message to stderr in production, or to stdout in development.
"""
function log_error(message::String)
    if is_production()
        println(stderr, "[", Dates.now(), "] ERROR: ", message)
    else
        println("[", Dates.now(), "] ERROR: ", message)
    end
end

"""
    log_info(message::String)

Log info message only in non-production environments.
"""
function log_info(message::String)
    if !is_production()
        println("[", Dates.now(), "] INFO: ", message)
    end
end

# Production Configuration
const PORT = get_environment_port()
const HOST = get_environment_host()

# Create router with production settings
const router = HTTP.Router()

"""
    root_handler(req::HTTP.Request) -> HTTP.Response

Root endpoint handler.
"""
function root_handler(req::HTTP.Request)::HTTP.Response
    return HTTP.Response(200, ""; headers=Dict("Content-Type" => "text/plain"))
end

"""
    user_id_handler(req::HTTP.Request) -> HTTP.Response

Get user by ID endpoint handler.
"""
function user_id_handler(req::HTTP.Request)::HTTP.Response
    id = HTTP.getparams(req)["id"]
    return HTTP.Response(200, id; headers=Dict("Content-Type" => "text/plain"))
end

"""
    create_user_handler(req::HTTP.Request) -> HTTP.Response

Create user endpoint handler.
"""
function create_user_handler(req::HTTP.Request)::HTTP.Response
    return HTTP.Response(200, ""; headers=Dict("Content-Type" => "text/plain"))
end

"""
    health_check_handler(req::HTTP.Request) -> HTTP.Response

Health check endpoint for monitoring.
"""
function health_check_handler(req::HTTP.Request)::HTTP.Response
    return HTTP.Response(200, "OK"; headers=Dict("Content-Type" => "text/plain"))
end

# Register routes
HTTP.register!(router, "GET", "/", root_handler)
HTTP.register!(router, "GET", "/user/{id}", user_id_handler)
HTTP.register!(router, "POST", "/user", create_user_handler)
HTTP.register!(router, "GET", "/health", health_check_handler)

# 404 handler
HTTP.register!(router, "*", "/", function(req::HTTP.Request)
    return HTTP.Response(404, "Not Found"; headers=Dict("Content-Type" => "text/plain"))
end)

# Create server with production settings
const server = try
    HTTP.serve(router, HOST, PORT; 
               server_header=false,
               date_header=true,
               retry=true,
               verbose=!is_production())
catch e
    log_error("Failed to start server: ", string(e))
    rethrow(e)
end

# Log server startup
log_info("HTTP.jl benchmark server listening on ", HOST, ":", PORT)

# Graceful shutdown handling
function handle_shutdown(signum::Int)
    log_info("Received signal ", signum, ". Shutting down gracefully...")
    try
        HTTP.close(server)
        log_info("Server stopped successfully")
    catch e
        log_error("Error stopping server: ", string(e))
    finally
        exit(0)
    end
end

# Register signal handlers
signal(Sys.SIGTERM, handle_shutdown)
signal(Sys.SIGINT, handle_shutdown)

# Keep server running
while isopen(server.socket)
    try
        sleep(1)
    catch e
        if !(e isa InterruptException)
            log_error("Server error: ", string(e))
        end
    end
end

log_info("Server has been stopped")
