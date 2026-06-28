/// <summary>
/// Production-grade Oxpecker web application
/// Implements security best practices with Oxpecker's functional approach
/// </summary>
module Program

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Oxpecker

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
    builder.AddFilter("Oxpecker", LogLevel.Information)
    builder.AddConsole(fun options ->
        options.TimestampFormat <- "yyyy-MM-dd HH:mm:ss "
        options.Format <- System.Formatter(`{ Timestamp = "yyyy-MM-dd HH:mm:ss "; LogLevel = @l; Message = @m; Exception = @x }`)
    )

/// <summary>
/// Configures services
/// </summary>
let configureServices (services: IServiceCollection) =
    services.AddRouting() |> ignore
    services.AddOxpecker() |> ignore
    services.AddMemoryCache() |> ignore
    services.AddHealthChecks() |> ignore

/// <summary>
/// Configures the application pipeline
/// </summary>
let configureApp (app: IApplicationBuilder) =
    app
        .UseMiddleware(errorHandlingMiddleware)
        .UseMiddleware(securityHeadersMiddleware)
        .UseRouting()
        .UseOxpecker(endpoints)

// ===========================================================================
// Endpoint Handlers
// ===========================================================================

/// <summary>
/// Empty handler for health check
/// </summary>
let emptyHandler: EndpointHandler = 
    fun ctx ->
        ctx.Response.StatusCode <- 200
        ctx.Response.ContentType <- "text/plain; charset=utf-8"
        Task.CompletedTask

/// <summary>
/// User ID handler
/// </summary>
let userIdHandler: EndpointHandler =
    fun ctx ->
        let id = ctx.Request.RouteValues["id"] :?> string
        ctx.Response.StatusCode <- 200
        ctx.Response.ContentType <- "text/plain; charset=utf-8"
        ctx.WriteText(id)

/// <summary>
/// User creation handler
/// </summary>
let userCreateHandler: EndpointHandler =
    fun ctx ->
        ctx.Response.StatusCode <- 201
        ctx.Response.ContentType <- "text/plain; charset=utf-8"
        Task.CompletedTask

// ===========================================================================
// Endpoint Definitions
// ===========================================================================

/// <summary>
/// Application endpoints configuration
/// </summary>
let endpoints = [
    GET [
        route "/" emptyHandler
        route "/user/{id}" userIdHandler
    ]
    POST [
        route "/user" userCreateHandler
    ]
]

// ===========================================================================
// Entry Point
// ===========================================================================

let args = System.Environment.GetCommandLineArgs()

Host
    .CreateDefaultBuilder(args)
    .ConfigureWebHost(fun webHost ->
        webHost
            .UseKestrel(configureKestrel)
            .ConfigureLogging(configureLogging)
            .ConfigureServices(configureServices)
            .Configure(configureApp)
        |> ignore)
    .Build()
    .Run()
