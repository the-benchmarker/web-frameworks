using EffinitiveFramework.Core;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.HttpOverrides;
using Microsoft.Extensions.Logging;
using System.Net;

// ============================================================================
// Production-Grade Effinitive Framework Configuration
// Best Practices: Security, Performance, Maintainability
// ============================================================================

// Configure Effinitive application with production settings
var app = EffinitiveApp
    .Create()
    .UsePort(int.Parse(Environment.GetEnvironmentVariable("PORT") ?? "3000"))
    .ConfigureKestrel(serverOptions =>
    {
        // Security: Remove server header
        serverOptions.AddServerHeader = false;
        
        // Performance: Configure limits
        serverOptions.Limits.MaxRequestBodySize = 16 * 1024 * 1024; // 16 MB
        serverOptions.Limits.MaxConcurrentConnections = null;
        serverOptions.Limits.MaxConcurrentUpgradedConnections = null;
        serverOptions.Limits.KeepAliveTimeout = TimeSpan.FromSeconds(75);
        serverOptions.Limits.RequestHeadersTimeout = TimeSpan.FromSeconds(30);
    })
    .ConfigureLogging(config =>
    {
        // Logging: Production minimal logging
        config.ClearProviders();
        config.AddConsole();
        config.AddFilter("Microsoft", LogLevel.Warning);
        config.AddFilter("System", LogLevel.Warning);
        config.AddFilter("Microsoft.AspNetCore", LogLevel.Warning);
        config.AddFilter("Microsoft", LogLevel.Debug, LogLevel.None);
    })
    .ConfigureServices(services =>
    {
        // Security services
        services.AddAntiforgery();
        
        // Performance services
        services.AddResponseCaching();
        services.AddResponseCompression();
    })
    .UseEnvironment(Environments.Production)
    .MapEndpoints()
    .Build();

// ============================================================================
// Security Headers Middleware
// ============================================================================
app.Use(async (context, next) =>
{
    context.Response.Headers.Append("X-Content-Type-Options", "nosniff");
    context.Response.Headers.Append("X-Frame-Options", "DENY");
    context.Response.Headers.Append("X-XSS-Protection", "1; mode=block");
    context.Response.Headers.Append("Referrer-Policy", "strict-origin-when-cross-origin");
    context.Response.Headers.Append("Permissions-Policy", "geolocation=(), microphone=(), camera=()");
    
    // Performance: Add cache-control headers
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

// ============================================================================
// Response Compression
// ============================================================================
app.UseResponseCompression();

// ============================================================================
// Exception Handling
// ============================================================================
app.UseExceptionHandler(errorApp =>
{
    errorApp.Run(async context =>
    {
        var exceptionHandlerFeature = context.Features.Get<IExceptionHandlerFeature>();
        
        if (exceptionHandlerFeature != null)
        {
            var logger = context.Logger;
            logger.LogError(exceptionHandlerFeature.Error, "Unhandled exception");
        }

        context.Response.StatusCode = (int)HttpStatusCode.InternalServerError;
        context.Response.ContentType = "text/plain";
        context.Response.Headers.Append("Cache-Control", "no-cache");
        await context.Response.WriteAsync("");
    });
});

// ============================================================================
// 404 Handler
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

// Run application
await app.RunAsync();

/// <summary>
/// Root endpoint - GET /
/// Returns empty response for benchmarking
/// </summary>
public class RootEndpoint : NoRequestEndpointBase<string>
{
    protected override string Method => "GET";
    protected override string Route => "/";
    protected override string ContentType => "text/plain";

    public override ValueTask<string> HandleAsync(CancellationToken cancellationToken = default)
    {
        // Security: Set cache headers
        HttpContext?.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        
        return ValueTask.FromResult(string.Empty);
    }
}

/// <summary>
/// Get user by ID endpoint - GET /user/{id}
/// Returns the ID for benchmarking
/// </summary>
public class GetUserEndpoint : NoRequestEndpointBase<string>
{
    protected override string Method => "GET";
    protected override string Route => "/user/{id}";
    protected override string ContentType => "text/plain";

    public override ValueTask<string> HandleAsync(CancellationToken cancellationToken = default)
    {
        // Security: Validate input
        var id = HttpContext?.RouteValues?["id"]?.ToString();
        
        if (string.IsNullOrWhiteSpace(id))
        {
            HttpContext.Response.StatusCode = 400;
            return ValueTask.FromResult(string.Empty);
        }
        
        // Security: Set cache headers
        HttpContext?.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        
        return ValueTask.FromResult(id);
    }
}

/// <summary>
/// Create user endpoint - POST /user
/// Returns empty response for benchmarking
/// </summary>
public class CreateUserEndpoint : NoRequestEndpointBase<string>
{
    protected override string Method => "POST";
    protected override string Route => "/user";
    protected override string ContentType => "text/plain";

    public override ValueTask<string> HandleAsync(CancellationToken cancellationToken = default)
    {
        // Security: Set cache headers
        HttpContext?.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        
        return ValueTask.FromResult(string.Empty);
    }
}
