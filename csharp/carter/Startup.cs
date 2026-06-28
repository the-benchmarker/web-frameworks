using Carter;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.HttpOverrides;
using Microsoft.Extensions.DependencyInjection;
using System.Net;

namespace web
{
    /// <summary>
    /// Startup configuration for Carter application
    /// Production-grade setup with security and performance best practices
    /// </summary>
    public class Startup
    {
        /// <summary>
        /// Configure services for dependency injection
        /// </summary>
        /// <param name="services">Service collection</param>
        public void ConfigureServices(IServiceCollection services)
        {
            // Add Carter framework
            services.AddCarter();
            
            // Security services
            services.AddAntiforgery();
            
            // Performance services
            services.AddResponseCaching();
            services.AddResponseCompression();
        }

        /// <summary>
        /// Configure the HTTP request pipeline
        /// </summary>
        /// <param name="app">Application builder</param>
        public void Configure(IApplicationBuilder app)
        {
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

            // Performance: Enable response compression
            app.UseResponseCompression();

            // Routing
            app.UseRouting();

            // Exception handling
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

            // Carter endpoints
            app.UseEndpoints(builder => builder.MapCarter());

            // 404 handler
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
        }
    }
}
