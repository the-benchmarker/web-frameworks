/// <summary>
/// Production-grade Giraffe web application
/// Implements security best practices, proper error handling, and performance optimizations
/// </summary>
module Program

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Giraffe

// ===========================================================================
// Security Headers Middleware
// ===========================================================================
/// <summary>
/// Adds security headers to all HTTP responses
/// Implements OWASP security best practices
/// </summary>
let securityHeadersMiddleware (next: HttpFunc) (ctx: HttpContext) =
    task {
        // Remove server header to avoid information disclosure
        ctx.Response.Headers.Remove("Server")
        
        // Security headers
        ctx.Response.Headers.Add("X-Content-Type-Options", "nosniff")
        ctx.Response.Headers.Add("X-Frame-Options", "DENY")
        ctx.Response.Headers.Add("X-XSS-Protection", "1; mode=block")
        ctx.Response.Headers.Add("Strict-Transport-Security", "max-age=63072000; includeSubDomains; preload")
        ctx.Response.Headers.Add("Content-Security-Policy", "default-src 'self'")
        ctx.Response.Headers.Add("Referrer-Policy", "strict-origin-when-cross-origin")
        ctx.Response.Headers.Add("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
        ctx.Response.Headers.Add("Cache-Control", "no-store, no-cache, must-revalidate, private")
        
        // Call next handler
        return! next ctx
    }

// ===========================================================================
// Error Handling Middleware
// ===========================================================================
/// <summary>
/// Handles exceptions and returns proper error responses
/// </summary>
let errorHandlingMiddleware (next: HttpFunc) (ctx: HttpContext) =
    task {
        try
            return! next ctx
        with ex ->
            ctx.Response.StatusCode <- 500
            ctx.Response.ContentType <- "text/plain; charset=utf-8"
            do! ctx.Response.WriteAsync("Internal Server Error")
    }

// ===========================================================================
// Web Application Configuration
// ===========================================================================

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
    builder.AddFilter("Giraffe", LogLevel.Information)
    
    // Add console logging for structured output
    builder.AddConsole(fun options ->
        options.TimestampFormat <- "yyyy-MM-dd HH:mm:ss "
        options.Format <- System.Formatter(`{ Timestamp = "yyyy-MM-dd HH:mm:ss "; LogLevel = @l; Message = @m; Exception = @x }`)
    )

/// <summary>
/// Configures services for the application
/// </summary>
let configureServices (services: IServiceCollection) =
    // Add Giraffe
    services.AddGiraffe()
    
    // Add memory cache for better performance
    services.AddMemoryCache() |> ignore
    
    // Add health checks
    services.AddHealthChecks() |> ignore

// ===========================================================================
// Web App Routing
// ===========================================================================

/// <summary>
/// Main web application with all routes
/// </summary>
let webApp : HttpFunc -> HttpFunc =
    choose [
        // Health check endpoint
        route "/" >=> GET >=> setStatusCode 200 >=> text ""
        
        // User retrieval by ID
        routef "/user/%s" (fun id -> GET >=> text id)
        
        // User creation
        route "/user" >=> POST >=> setStatusCode 201 >=> text ""
        
        // 404 Not Found
        setStatusCode 404 >=> text "Not Found"
    ]

// ===========================================================================
// Application Pipeline Configuration
// ===========================================================================

/// <summary>
/// Configures the HTTP request pipeline
/// </summary>
let configureApp (app: IApplicationBuilder) =
    // Order matters!
    // 1. Exception handling
    app.UseMiddleware(errorHandlingMiddleware)
        .UseMiddleware(securityHeadersMiddleware)
        .UseRouting()
        .UseGiraffe(webApp)

// ===========================================================================
// Entry Point
// ===========================================================================

let args = System.Environment.GetCommandLineArgs()

Host.CreateDefaultBuilder(args)
    .ConfigureWebHost(fun webHost ->
        webHost
            .UseKestrel(configureKestrel)
            .ConfigureLogging(configureLogging)
            .ConfigureServices(configureServices)
            .Configure(Action<IApplicationBuilder> configureApp)
            |> ignore)
    .Build()
    .Run()
