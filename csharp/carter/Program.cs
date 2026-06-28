using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.AspNetCore.HttpOverrides;

namespace web
{
    /// <summary>
    /// Production-grade Carter application entry point
    /// Implements security, performance, and maintainability best practices
    /// </summary>
    public class Program
    {
        /// <summary>
        /// Main entry point for the application
        /// </summary>
        /// <param name="args">Command line arguments</param>
        public static void Main(string[] args)
        {
            Host.CreateDefaultBuilder(args)
                .ConfigureWebHostDefaults(webBuilder =>
                {
                    // Security: Remove server header
                    webBuilder.ConfigureKestrel(serverOptions =>
                    {
                        serverOptions.AddServerHeader = false;
                        
                        // Performance: Configure limits
                        serverOptions.Limits.MaxRequestBodySize = 16 * 1024 * 1024; // 16 MB
                        serverOptions.Limits.MaxConcurrentConnections = null;
                        serverOptions.Limits.MaxConcurrentUpgradedConnections = null;
                        serverOptions.Limits.KeepAliveTimeout = TimeSpan.FromSeconds(75);
                        serverOptions.Limits.RequestHeadersTimeout = TimeSpan.FromSeconds(30);
                    });

                    // Logging: Production minimal logging
                    webBuilder.ConfigureLogging(config =>
                    {
                        config.ClearProviders();
                        config.AddConsole();
                        config.AddFilter("Microsoft", LogLevel.Warning);
                        config.AddFilter("System", LogLevel.Warning);
                        config.AddFilter("Microsoft.AspNetCore", LogLevel.Warning);
                        config.AddFilter("Microsoft", LogLevel.Debug, LogLevel.None);
                    });

                    // Configure for production environment
                    webBuilder.UseEnvironment(Environments.Production);
                    
                    // Handle forwarded headers
                    webBuilder.ConfigureAppConfiguration((hostingContext, config) =>
                    {
                        // Add any additional configuration if needed
                    });

                    webBuilder.UseStartup<Startup>();
                })
                .Build()
                .Run();
        }
    }
}
