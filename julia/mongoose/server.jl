"""
Mongoose.jl Benchmark Server

Production-grade benchmark server implementation using Mongoose.jl framework.

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

using Mongoose
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

# Create server
const server = Server()

"""
    getroot(request::Request) -> Response

Root endpoint handler.
Optimized for minimal latency and maximum throughput.

# Arguments
- `request::Request`: The Mongoose Request object

# Returns
- `Response`: Empty response with 200 status
"""
function getroot(request::Request)
    return Response(""; headers=Dict("Content-Type" => "text/plain"))
end

"""
    getuserid(request::Request, id::String) -> Response

Get user by ID endpoint handler.
Optimized endpoint that returns the user ID as plain text.

# Arguments
- `request::Request`: The Mongoose Request object
- `id::String`: The user ID from path

# Returns
- `Response`: User ID as plain text with 200 status
"""
function getuserid(request::Request, id::String)
    return Response(id; headers=Dict("Content-Type" => "text/plain"))
end

"""
    postuser(request::Request) -> Response

Create user endpoint handler.
Optimized POST endpoint for creating users.

# Arguments
- `request::Request`: The Mongoose Request object

# Returns
- `Response`: Empty response with 200 status
"""
function postuser(request::Request)
    return Response(""; headers=Dict("Content-Type" => "text/plain"))
end

"""
    health_check(request::Request) -> Response

Health check endpoint for monitoring.
Production health check endpoint used by monitoring systems.

# Arguments
- `request::Request`: The Mongoose Request object

# Returns
- `Response`: Health status with 200 status
"""
function health_check(request::Request)
    return Response("OK"; headers=Dict("Content-Type" => "text/plain"))
end

# Register routes
route!(server, :get, "/", getroot)
route!(server, :get, "/user/:id", getuserid)
route!(server, :post, "/user", postuser)
route!(server, :get, "/health", health_check)

# Start server with production settings
try
    # Start server without verbose logging in production
    start!(server, host=HOST, port=PORT)
    
    log_info("Mongoose.jl benchmark server listening on ", HOST, ":", PORT)
    
    # Graceful shutdown handling
    function handle_shutdown(signum::Int)
        log_info("Received signal ", signum, ". Shutting down gracefully...")
        try
            stop!(server)
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
    while true
        try
            sleep(1)
        catch e
            if !(e isa InterruptException)
                log_error("Server error: ", string(e))
            else
                break
            end
        end
    end
    
except e
    log_error("Failed to start server: ", string(e))
    rethrow(e)
end
