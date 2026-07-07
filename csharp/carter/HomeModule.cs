using System.Threading.Tasks;
using Carter;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Routing;

namespace web
{
    /// <summary>
    /// Home endpoint module for Carter
    /// Handles root endpoint for benchmarking
    /// </summary>
    public class HomeModule : CarterModule
    {
        /// <summary>
        /// Configure routes for this module
        /// </summary>
        /// <param name="app">Endpoint route builder</param>
        public override void AddRoutes(IEndpointRouteBuilder app)
        {
            // GET / - Root endpoint for benchmarking
            app.MapGet("/", async context =>
            {
                context.Response.ContentType = "text/plain";
                context.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
                await context.Response.WriteAsync("");
            });
        }
    }
}
