using System.Threading.Tasks;
using Carter;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Routing;

namespace web
{
    /// <summary>
    /// User endpoint module for Carter
    /// Handles user-related endpoints for benchmarking
    /// </summary>
    public class UserModule : CarterModule
    {
        /// <summary>
        /// Configure routes for user operations
        /// </summary>
        /// <param name="app">Endpoint route builder</param>
        public override void AddRoutes(IEndpointRouteBuilder app)
        {
            // GET /user/{id} - Get user by ID
            app.MapGet("/user/{id}", async ([FromRoute] string id, HttpContext context) =>
            {
                // Security: Validate input
                if (string.IsNullOrWhiteSpace(id))
                {
                    context.Response.StatusCode = 400;
                    return;
                }
                
                context.Response.ContentType = "text/plain";
                context.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
                await context.Response.WriteAsync(id);
            });

            // POST /user - Create user
            app.MapPost("/user", async (HttpContext context) =>
            {
                context.Response.ContentType = "text/plain";
                context.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
                context.Response.StatusCode = 200;
                await context.Response.WriteAsync("");
            });
        }
    }
}
