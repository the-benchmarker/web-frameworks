"""
Oxygen.jl Benchmark Server

Production-grade benchmark server implementation using Oxygen.jl framework.

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

using Oxygen
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

"""
    root_handler() -> String

Root endpoint handler.
"""
@get "/" function root_handler()
    return text(""; headers=Dict("Content-Type" => "text/plain"))
end

"""
    user_id_handler(request::HTTP.Request, id::String) -> String

Get user by ID endpoint handler.
"""
@get "/user/{id}" function user_id_handler(request::HTTP.Request, id::String)
    return text(id; headers=Dict("Content-Type" => "text/plain"))
end

"""
    create_user_handler() -> String

Create user endpoint handler.
"""
@post "/user" function create_user_handler()
    return text(""; headers=Dict("Content-Type" => "text/plain"))
end

"""
    health_check_handler() -> String

Health check endpoint for monitoring.
"""
@get "/health" function health_check_handler()
    return text("OK"; headers=Dict("Content-Type" => "text/plain"))
end

# 404 handler
@get "*" function not_found_handler()
    return text("Not Found"; status=404, headers=Dict("Content-Type" => "text/plain"))
end

# Start server with production settings
try
    serveparallel(
        host=HOST,
        port=PORT,
        docs=false,
        metrics=false,
        access_log=is_production() ? nothing : stdout
    )
    
    log_info("Oxygen.jl benchmark server listening on ", HOST, ":", PORT)
    
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
