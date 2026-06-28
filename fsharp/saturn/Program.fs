/// <summary>
/// Production-grade Saturn web application
/// Implements security best practices with Saturn's opinionated approach
/// </summary>
module Program

open System
open Giraffe.ResponseWriters
open Giraffe.Core
open Saturn
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging

// ===========================================================================
// Security Headers Middleware
// ===========================================================================
/// <summary>
/// Adds security headers to all HTTP responses
/// </summary>
let securityHeadersMiddleware (next: RequestDelegate) (ctx: HttpContext) =
    task {
        ctx.Response.Headers.Remove("Server")
        ctx.Response.Headers.Add("X-Content-Type-Options", "nosniff")
        ctx.Response.Headers.Add("X-Frame-Options", "DENY")
        ctx.Response.Headers.Add("X-XSS-Protection", "1; mode=block")
        ctx.Response.Headers.Add("Strict-Transport-Security", "max-age=63072000; includeSubDomains; preload")
        ctx.Response.Headers.Add("Content-Security-Policy", "default-src 'self'")
        ctx.Response.Headers.Add("Referrer-Policy", "strict-origin-when-cross-origin")
        ctx.Response.Headers.Add("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
        ctx.Response.Headers.Add("Cache-Control", "no-store, no-cache, must-revalidate, private")
        do! next.Invoke(ctx)
    }

// ===========================================================================
// Error Handling Middleware
// ===========================================================================
/// <summary>
/// Handles exceptions globally
/// </summary>
let errorHandlingMiddleware (next: RequestDelegate) (ctx: HttpContext) =
    task {
        try
            do! next.Invoke(ctx)
        with _ ->
            ctx.Response.StatusCode <- 500
            ctx.Response.ContentType <- "text/plain; charset=utf-8"
            do! ctx.Response.WriteAsync("Internal Server Error")
    }

// ===========================================================================
// Configuration Functions
// ===========================================================================

/// <summary>
/// Configures Kestrel with production settings
/// </summary>
let configureKestrel (options: KestrelServerOptions) =
    options.ListenAnyIP(3000) |> ignore
    options.Limits.MaxRequestBodySize <- 8L * 1024L * 1024L
    options.Limits.KeepAliveTimeout <- TimeSpan.FromSeconds(60.0)
    options.Limits.RequestHeadersTimeout <- TimeSpan.FromSeconds(30.0)
    options.AddServerHeader <- false
    options.Limits.MaxConcurrentConnections <- 16384
    options.Limits.MaxConcurrentUpgradedConnections <- 1000

/// <summary>
/// Configures logging for production
/// </summary>
let configureLogging (builder: ILoggingBuilder) =
    builder.ClearProviders()
    builder.AddFilter("Microsoft", LogLevel.Warning)
    builder.AddFilter("System", LogLevel.Warning)
    builder.AddFilter("Saturn", LogLevel.Information)
    builder.AddConsole(fun options ->
        options.TimestampFormat <- "yyyy-MM-dd HH:mm:ss "
        options.Format <- System.Formatter(`{ Timestamp = "yyyy-MM-dd HH:mm:ss "; LogLevel = @l; Message = @m; Exception = @x }`)
    )

/// <summary>
/// Configures services for production
/// </summary>
let configureServices (services: IServiceCollection) =
    services.AddMemoryCache() |> ignore
    services.AddHealthChecks() |> ignore

// ===========================================================================
// Router Configuration
// ===========================================================================

/// <summary>
/// Main application router with all endpoints
/// </summary>
let topRouter = router {
    // Health check endpoint
    get "/" (setStatusCode 200 >=> text "")
    
    // User retrieval by ID
    getf "/user/%s" text
    
    // User creation
    post "/user" (setStatusCode 201 >=> text "")
    
    // 404 Not Found
    not_found_handler (setStatusCode 404 >=> text "Not Found")
}

// ===========================================================================
// Application Configuration
// ===========================================================================

/// <summary>
/// Saturn application configuration
/// </summary>
let app = application {
    // Router configuration
    use_router topRouter
    
    // Error handling
    error_handler (fun ex ctx ->
        ctx.Response.StatusCode <- 500
        ctx.Response.ContentType <- "text/plain; charset=utf-8"
        ctx.Response.WriteAsync("Internal Server Error")
    )
    
    // Not found handler
    not_found_handler (setStatusCode 404 >=> text "Not Found")
    
    // Logging configuration for production
    logging (fun logger ->
        configureLogging logger
    )
    
    // Kestrel configuration
    url "http://*:3000"
}

// ===========================================================================
// Entry Point
// ===========================================================================

[<EntryPoint>]
let main args =
    // Configure services before running
    let builder = WebApplication.CreateBuilder(args)
    configureServices builder.Services
    configureLogging builder.Logging
    
    // Run Saturn application
    run app
    
    0
