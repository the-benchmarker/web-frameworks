using FastEndpoints;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.HttpOverrides;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using System.Net;

// ============================================================================
// Production-Grade FastEndpoints Configuration
// Best Practices: Security, Performance, Maintainability
// ============================================================================

// Configure application builder with production settings
var builder = WebApplication.CreateBuilder(new WebApplicationOptions
{
    Args = args,
    ContentRootPath = AppContext.BaseDirectory,
    EnvironmentName = Environments.Production // Force production environment
});

// ============================================================================
// Kestrel Configuration - Production Optimized
// ============================================================================
builder.WebHost.ConfigureKestrel(serverOptions =>
{
    // Security: Remove server header to prevent information disclosure
    serverOptions.AddServerHeader = false;

    // Performance: Configure limits for production workloads
    serverOptions.Limits.MaxRequestBodySize = 16 * 1024 * 1024; // 16 MB
    serverOptions.Limits.MaxConcurrentConnections = null; // No artificial limit
    serverOptions.Limits.MaxConcurrentUpgradedConnections = null;
    serverOptions.Limits.KeepAliveTimeout = TimeSpan.FromSeconds(75);
    serverOptions.Limits.RequestHeadersTimeout = TimeSpan.FromSeconds(30);
});

// ============================================================================
// Logging Configuration - Production Minimal
// ============================================================================
// Disable all logging for maximum performance (as noted in comment)
builder.Logging.ClearProviders();
// Only enable console logging with minimal level (Warning+ only)
builder.Logging.AddConsole();
builder.Logging.AddFilter("Microsoft", LogLevel.Warning);
builder.Logging.AddFilter("System", LogLevel.Warning);
builder.Logging.AddFilter("Microsoft.AspNetCore", LogLevel.Warning);
// Disable debug logging entirely
builder.Logging.AddFilter("Microsoft", LogLevel.Debug, LogLevel.None);

// ============================================================================
// Services Configuration
// ============================================================================
// Add essential production services
builder.Services.AddAntiforgery(); // Security: Anti-forgery tokens
builder.Services.AddResponseCaching(); // Performance: Response caching
builder.Services.AddResponseCompression(); // Performance: Compression

// Add FastEndpoints framework
builder.Services.AddFastEndpoints();

// ============================================================================
// Build Application
// ============================================================================
var app = builder.Build();

// ============================================================================
// Security Middleware Pipeline
// ============================================================================

// Security: Redirect HTTP to HTTPS (disabled for benchmarking to avoid overhead)
// app.UseHttpsRedirection();

// Security: Handle forwarded headers (behind reverse proxy)
app.UseForwardedHeaders(new ForwardedHeadersOptions
{
    ForwardedHeaders = ForwardedHeaders.XForwardedFor | ForwardedHeaders.XForwardedProto
});

// Security: Add security headers middleware
app.Use(async (context, next) =>
{
    context.Response.Headers.Append("X-Content-Type-Options", "nosniff");
    context.Response.Headers.Append("X-Frame-Options", "DENY");
    context.Response.Headers.Append("X-XSS-Protection", "1; mode=block");
    context.Response.Headers.Append("Referrer-Policy", "strict-origin-when-cross-origin");
    context.Response.Headers.Append("Permissions-Policy", "geolocation=(), microphone=(), camera=()");

    // Performance: Add cache-control headers for static content
    if (context.Request.Path.StartsWithSegments("/static"))
    {
        context.Response.Headers.Append("Cache-Control", "public,max-age=3600");
    }
    else
    {
        context.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
    }

    await next(context);
});

// Performance: Enable response compression
app.UseResponseCompression();

// ============================================================================
// FastEndpoints Middleware
// ============================================================================
app.UseFastEndpoints();

// ============================================================================
// Exception Handling - Production Ready
// ============================================================================
app.UseExceptionHandler(errorApp =>
{
    errorApp.Run(async context =>
    {
        var exceptionHandlerFeature = context.Features.Get<IExceptionHandlerFeature>();
        
        if (exceptionHandlerFeature != null)
        {
            // Log only error level, not debug
            var logger = context.Logger;
            logger.LogError(exceptionHandlerFeature.Error, "Unhandled exception");
        }

        // For benchmarking: return minimal error response
        context.Response.StatusCode = (int)HttpStatusCode.InternalServerError;
        context.Response.ContentType = "text/plain";
        context.Response.Headers.Append("Cache-Control", "no-cache");
        await context.Response.WriteAsync("");
    });
});

// ============================================================================
// 404 Handler - Production Ready
// ============================================================================
app.Use(async (context, next) =>
{
    await next(context);

    if (context.Response.StatusCode == (int)HttpStatusCode.NotFound)
    {
        context.Response.ContentType = "text/plain";
        context.Response.Headers.Append("Cache-Control", "no-cache");
        await context.Response.WriteAsync("Not Found");
    }
});

// ============================================================================
// Application Runtime Configuration
// ============================================================================
// Get configuration from environment variables
var port = int.Parse(Environment.GetEnvironmentVariable("PORT") ?? "3000");
var host = Environment.GetEnvironmentVariable("HOST") ?? "0.0.0.0";

// Run application
app.Run($"{host}:{port}");

/// <summary>
/// Home endpoint - GET /
/// Returns empty response for benchmarking
/// </summary>
public class HomeEndpoint : EndpointWithoutRequest
{
    /// <summary>
    /// Configure endpoint settings
    /// </summary>
    public override void Configure()
    {
        Get("/");
        AllowAnonymous();
        // Performance: Disable model binding and validation for benchmarking
        DisableAutoDummyModelBinding();
    }

    /// <summary>
    /// Handle GET request for root endpoint
    /// </summary>
    public override Task HandleAsync(CancellationToken cancellationToken)
    {
        // Security: Set cache headers
        HttpContext.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        HttpContext.Response.ContentType = "text/plain";
        return HttpContext.Response.WriteAsync("");
    }
}

/// <summary>
/// User by ID endpoint - GET /user/{id}
/// Returns the ID for benchmarking
/// </summary>
public class UserByIdEndpoint : EndpointWithoutRequest
{
    /// <summary>
    /// Configure endpoint settings
    /// </summary>
    public override void Configure()
    {
        Get("/user/{id}");
        AllowAnonymous();
        // Performance: Disable model binding and validation for benchmarking
        DisableAutoDummyModelBinding();
    }

    /// <summary>
    /// Handle GET request for user by ID
    /// </summary>
    public override Task HandleAsync(CancellationToken cancellationToken)
    {
        // Security: Validate input
        var id = HttpContext.Request.RouteValues["id"]?.ToString();
        
        if (string.IsNullOrWhiteSpace(id))
        {
            HttpContext.Response.StatusCode = StatusCodes.Status400BadRequest;
            return HttpContext.Response.WriteAsync("");
        }
        
        // Security: Set cache headers
        HttpContext.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        HttpContext.Response.ContentType = "text/plain";
        return HttpContext.Response.WriteAsync(id);
    }
}

/// <summary>
/// Create user endpoint - POST /user
/// Returns empty response for benchmarking
/// </summary>
public class CreateUserEndpoint : EndpointWithoutRequest
{
    /// <summary>
    /// Configure endpoint settings
    /// </summary>
    public override void Configure()
    {
        Post("/user");
        AllowAnonymous();
        Description(x => x.Accepts<object>("*/*"));
        // Performance: Disable model binding and validation for benchmarking
        DisableAutoDummyModelBinding();
    }

    /// <summary>
    /// Handle POST request for user creation
    /// </summary>
    public override Task HandleAsync(CancellationToken cancellationToken)
    {
        // Security: Set cache headers
        HttpContext.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        HttpContext.Response.ContentType = "text/plain";
        return HttpContext.Response.WriteAsync("");
    }
}
