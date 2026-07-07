/// <summary>
/// Production-grade Frank web application
/// Implements security best practices, proper error handling, and performance optimizations
/// </summary>
module Program

open System
open Frank.Builder
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open System.Threading.Tasks

/// <summary>
/// Adds security headers middleware to the pipeline
/// </summary>
let securityHeadersMiddleware (next: RequestDelegate) =
    fun (context: HttpContext) ->
        task {
            // Remove server header to avoid information disclosure
            context.Response.Headers.Remove("Server")
            
            // Security headers
            context.Response.Headers.Add("X-Content-Type-Options", "nosniff")
            context.Response.Headers.Add("X-Frame-Options", "DENY")
            context.Response.Headers.Add("X-XSS-Protection", "1; mode=block")
            context.Response.Headers.Add("Strict-Transport-Security", "max-age=63072000; includeSubDomains; preload")
            context.Response.Headers.Add("Content-Security-Policy", "default-src 'self'")
            context.Response.Headers.Add("Referrer-Policy", "strict-origin-when-cross-origin")
            context.Response.Headers.Add("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
            context.Response.Headers.Add("Cache-Control", "no-store, no-cache, must-revalidate, private")
            
            // Call next middleware
            do! next.Invoke(context)
        }

/// <summary>
/// Error handling middleware
/// </summary>
let errorHandlingMiddleware (next: RequestDelegate) =
    fun (context: HttpContext) ->
        task {
            try
                do! next.Invoke(context)
            with ex ->
                context.Response.StatusCode <- 500
                context.Response.ContentType <- "text/plain; charset=utf-8"
                do! context.Response.WriteAsync("Internal Server Error")
        }

/// <summary>
/// Configures Kestrel server with production-grade settings
/// </summary>
let configureKestrel (options: KestrelServerOptions) =
    // Listen on all interfaces
    options.ListenAnyIP(3000) |> ignore
    
    // Security: Limit request body size to prevent DoS
    options.Limits.MaxRequestBodySize <- 8L * 1024L * 1024L  // 8MB
    
    // Performance: Configure keep-alive
    options.Limits.KeepAliveTimeout <- TimeSpan.FromSeconds(60.0)
    options.Limits.RequestHeadersTimeout <- TimeSpan.FromSeconds(30.0)
    
    // Security: Disable server header
    options.AddServerHeader <- false
    
    // Performance: Connection limits
    options.Limits.MaxConcurrentConnections <- 16384
    options.Limits.MaxConcurrentUpgradedConnections <- 1000

/// <summary>
/// Configures logging for production
/// </summary>
let configureLogging (builder: ILoggingBuilder) =
    builder.ClearProviders()
    
    // Configure minimal logging for production
    builder.AddFilter("Microsoft", LogLevel.Warning)
    builder.AddFilter("System", LogLevel.Warning)
    builder.AddFilter("Frank", LogLevel.Information)
    
    // Add console logging for structured output
    builder.AddConsole(fun options ->
        options.TimestampFormat <- "yyyy-MM-dd HH:mm:ss "
        options.Format <- System.Formatter(`{ Timestamp = "yyyy-MM-dd HH:mm:ss "; LogLevel = @l; Message = @m; Exception = @x }`)
    )

/// <summary>
/// Configures services for the application
/// </summary>
let configureServices (services: IServiceCollection) =
    // Add memory cache for better performance
    services.AddMemoryCache() |> ignore
    
    // Add health checks
    services.AddHealthChecks() |> ignore

// ===========================================================================
// Resource Definitions
// ===========================================================================

let home =
    resource "/" {
        name "Home - Health Check"
        get (fun ctx ->
            ctx.Response.StatusCode <- 200
            ctx.Response.ContentType <- "text/plain; charset=utf-8"
            ctx.Response.WriteAsync("")
        )
    }

let userId =
    resource "/user/{id}" {
        name "Get User By ID"
        get (fun (ctx: HttpContext) ->
            task {
                let userId = ctx.GetRouteValue("id")
                ctx.Response.StatusCode <- 200
                ctx.Response.ContentType <- "text/plain; charset=utf-8"
                do! ctx.Response.WriteAsync(string userId)
            }
        )
    }

let user =
    resource "/user" {
        name "Create User"
        post (fun ctx ->
            ctx.Response.StatusCode <- 201
            ctx.Response.ContentType <- "text/plain; charset=utf-8"
            ctx.Response.WriteAsync("")
        )
    }

// ===========================================================================
// Application Configuration
// ===========================================================================

let args = System.Environment.GetCommandLineArgs()

webHost args {
    configure (fun bldr ->
        // Configure Kestrel
        bldr.UseKestrel(configureKestrel)
            .ConfigureLogging(configureLogging)
            .ConfigureServices(configureServices)
            .UseMiddleware(securityHeadersMiddleware)
            .UseMiddleware(errorHandlingMiddleware)
    )
    resource home
    resource userId
    resource user
}
