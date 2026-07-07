using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using System.Net;

// ============================================================================
// Production-Grade ASP.NET Core MVC Configuration
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
// Disable all debug and information logging for production performance
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

// Configure MVC with production settings
builder.Services.AddControllers(options =>
{
    options.RespectBrowserAcceptHeader = true;
    options.ReturnHttpNotAcceptable = true;
    options.SuppressAsyncSuffixInActionNames = false;
    options.SuppressModelStateInvalidFilter = true;
});

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
    ForwardedHeaders = Microsoft.AspNetCore.HttpOverrides.ForwardedHeaders.XForwardedFor |
                       Microsoft.AspNetCore.HttpOverrides.ForwardedHeaders.XForwardedProto
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
// Request Pipeline
// ============================================================================
app.UseRouting();

// Security: Enable authorization (if configured)
app.UseAuthorization();

// Map controllers
app.MapControllers();

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
