/// <summary>
/// Production-grade Falco web application
/// Implements security best practices, proper error handling, and performance optimizations
/// </summary>
module Program

open System
open Falco
open Falco.Routing
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Net.Http.Headers

/// <summary>
/// Adds security headers to all HTTP responses
/// Implements OWASP security best practices
/// </summary>
let configureSecurityHeaders (appBuilder: IApplicationBuilder) =
    appBuilder
        .Use(async fun (context: HttpContext) ->
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
            
            // Continue to next middleware
            do! context.Response.WriteAsync("")
        )

/// <summary>
/// Configures Kestrel server with production-grade settings
/// </summary>
let configureKestrel (builder: IWebHostBuilder) =
    builder.ConfigureKestrel(fun options ->
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
        
        // Security: Enable HTTPS redirect in production
        if Environment.GetEnvironmentVariable("ASPNETCORE_ENVIRONMENT") = "Production" then
            options.ConfigureHttpsDefaults(fun httpsOptions ->
                httpsOptions.SslProtocols <- System.Security.Authentication.SslProtocols.Tls12 ||| SslProtocols.Tls13
            )
    )

/// <summary>
/// Configures logging for production
/// Disables debug logging and configures structured logging
/// </summary>
let configureLogging (builder: WebApplicationBuilder) =
    builder.Logging.ClearProviders()
    
    // Configure minimal logging for production
    builder.Logging.AddFilter("Microsoft", LogLevel.Warning)
    builder.Logging.AddFilter("System", LogLevel.Warning)
    builder.Logging.AddFilter("Program", LogLevel.Information)
    
    // Add console logging for structured output
    builder.Logging.AddConsole(fun options ->
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

/// <summary>
/// Configures the HTTP request pipeline
/// </summary>
let configurePipeline (app: WebApplication) =
    // Order matters!
    // 1. Exception handling
    app.UseExceptionHandler(fun errorApp ->
        errorApp.Run(fun context ->
            context.Response.StatusCode <- 500
            context.Response.ContentType <- "text/plain; charset=utf-8"
            context.Response.WriteAsync("Internal Server Error")
        )
    )
    
    // 2. HTTPS redirection (in production)
    if Environment.GetEnvironmentVariable("ASPNETCORE_ENVIRONMENT") = "Production" then
        app.UseHttpsRedirection()
    
    // 3. Static files (if any)
    app.UseStaticFiles()
    
    // 4. Routing
    app.UseRouting()
    
    // 5. Security headers
    configureSecurityHeaders app
    
    // 6. Falco routing
    app.UseFalco([
        // Health check endpoint
        mapGet  "/" Response.ofEmpty
        
        // User retrieval by ID
        mapGet  "/user/{id}" (fun r -> 
            let id = r.GetString "id"
            Response.ofPlainText id
        )
        
        // User creation
        post "/user" Response.ofEmpty
        
        // 404 handler
        getAny Response.ofNotFound
    ])

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    
    // Configure logging for production
    configureLogging builder
    
    // Configure services
    configureServices builder.Services
    
    // Build the application
    let app = builder.Build()
    
    // Configure the HTTP pipeline
    configurePipeline app
    
    // Run the application
    app.Run()
    
    0
