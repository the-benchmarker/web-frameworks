using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using System.Net;

// Configure builder
var builder = WebApplication.CreateBuilder(new WebApplicationOptions
{
    Args = args,
    ContentRootPath = AppContext.BaseDirectory,
});

// Configure Kestrel for production
builder.WebHost.ConfigureKestrel(serverOptions =>
{
    serverOptions.AddServerHeader = false;
    serverOptions.Limits.MaxRequestBodySize = 16 * 1024 * 1024; // 16 MB
    serverOptions.Limits.MaxConcurrentConnections = null; // No limit
    serverOptions.Limits.MaxConcurrentUpgradedConnections = null;
    serverOptions.Limits.KeepAliveTimeout = TimeSpan.FromSeconds(75);
    serverOptions.Limits.RequestHeadersTimeout = TimeSpan.FromSeconds(30);
});

// Configure logging
builder.Logging.ClearProviders();
builder.Logging.AddConsole();
builder.Logging.AddFilter("Microsoft", LogLevel.Warning);
builder.Logging.AddFilter("System", LogLevel.Warning);
builder.Logging.AddFilter("Microsoft.AspNetCore", LogLevel.Debug);

// Build application
var app = builder.Build();

// Configure application
app.UseHttpsRedirection();
app.UseForwardedHeaders();

// Configure request pipeline
app.Use(async (context, next) =>
{
    // Log requests for debugging
    var logger = context.Logger;
    var start = DateTimeOffset.UtcNow;
    
    try
    {
        await next(context);
        var duration = DateTimeOffset.UtcNow - start;
        logger.LogDebug("{Method} {Path} {StatusCode} - {Duration}ms", 
            context.Request.Method, 
            context.Request.Path, 
            context.Response.StatusCode, 
            duration.TotalMilliseconds);
    }
    catch (Exception ex)
    {
        var duration = DateTimeOffset.UtcNow - start;
        logger.LogError(ex, "{Method} {Path} - {Duration}ms", 
            context.Request.Method, 
            context.Request.Path, 
            duration.TotalMilliseconds);
        throw;
    }
});

// Root endpoint
// GET /
app.MapGet("/", () =>
{
    return Results.Text("", "text/plain");
});

// Get user by ID endpoint
// GET /user/{id}
app.MapGet("/user/{id}", (string id) =>
{
    return Results.Text(id, "text/plain");
});

// Create user endpoint
// POST /user
app.MapPost("/user", () =>
{
    return Results.Text("", "text/plain");
});

// Health check endpoint for monitoring
// GET /health
app.MapGet("/health", () =>
{
    return Results.Text("OK", "text/plain");
});

// Global exception handler
app.UseExceptionHandler(errorApp =>
{
    errorApp.Run(async context =>
    {
        var exceptionHandlerFeature = context.Features.Get<IExceptionHandlerFeature>();
        if (exceptionHandlerFeature != null)
        {
            // Log the exception
            var logger = context.Logger;
            logger.LogError(exceptionHandlerFeature.Error, "Unhandled exception");
        }
        
        // For benchmarking, return empty response
        context.Response.StatusCode = (int)HttpStatusCode.InternalServerError;
        context.Response.ContentType = "text/plain";
        await context.Response.WriteAsync("");
    });
});

// 404 handler
app.Use(async (context, next) =>
{
    await next(context);
    
    if (context.Response.StatusCode == (int)HttpStatusCode.NotFound)
    {
        context.Response.ContentType = "text/plain";
        await context.Response.WriteAsync("Not Found");
    }
});

// Get port from environment or use default
var port = int.Parse(Environment.GetEnvironmentVariable("PORT") ?? "3000");
var host = Environment.GetEnvironmentVariable("HOST") ?? "0.0.0.0";

// Run application
app.Run($"{host}:{port}");
