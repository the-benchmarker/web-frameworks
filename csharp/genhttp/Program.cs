using GenHTTP.Engine.Internal;
using Microsoft.Extensions.Logging;

using web;

// ============================================================================
// Production-Grade GenHTTP Configuration
// Best Practices: Security, Performance, Maintainability
// ============================================================================

// Get configuration from environment variables
var port = int.Parse(Environment.GetEnvironmentVariable("PORT") ?? "3000");
var host = Environment.GetEnvironmentVariable("HOST") ?? "0.0.0.0";

// Create benchmark handler with production settings
var app = new BenchmarkHandler();

// Configure and run host with production settings
return await Host.Create()
                 .Handler(app)
                 .Port(port)
                 .Host(host)
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
                 .RunAsync();
