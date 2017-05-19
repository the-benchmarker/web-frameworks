using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.AspNetCore.Routing;

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
            var routeBuilder = new RouteBuilder(app);

            routeBuilder.MapGet("", context => {
                return context.Response.WriteAsync("");
            });

            routeBuilder.MapGet("user/{id}", context => {
                return context.Response.WriteAsync($"{context.GetRouteValue("id")}");
            });

            routeBuilder.MapPost("user", context => {
                return context.Response.WriteAsync("");
            });

            var routes = routeBuilder.Build();
            app.UseRouter(routes);
        }
    }
}
