using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Routing;
using Microsoft.Extensions.DependencyInjection;

namespace aspnetcore
{
    public class Startup
    {
        public void Configure(IApplicationBuilder app)
        {
            app.UseRouting();
            app.UseEndpoints(endpoints =>
            {
                endpoints.MapGet("", context => {
                    return context.Response.WriteAsync("");
                });

                endpoints.MapGet("user/{id}", context => {
                    var id = context.GetRouteValue("id").ToString();
                    return context.Response.WriteAsync(id);
                });

                endpoints.MapPost("user", context => {
                    return context.Response.WriteAsync("");
                });
            });
        }
    }
}
