using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Routing;
using Microsoft.Extensions.DependencyInjection;

namespace aspnetcore
{
  public class Startup
    {
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddRouting();
        }

        public void Configure(IApplicationBuilder app)
        {
            app.UseRouter(routes =>
            {
                routes.MapGet("", context => {
                    return context.Response.WriteAsync("");
                });

                routes.MapGet("user/{id}", context => {
                    var id = context.GetRouteValue("id").ToString();
                    return context.Response.WriteAsync(id);
                });

                routes.MapPost("user", context => {
                    return context.Response.WriteAsync("");
                });
            });
        }
    }
}
