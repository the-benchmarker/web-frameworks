using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Routing;
using Microsoft.Extensions.DependencyInjection;
using System.Threading.Tasks;

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
            app.UseRouting();
            app.UseEndpoints(endpoints =>
            {
                endpoints.MapGet("", context => Task.CompletedTask);

                endpoints.MapGet("user/{id}", async context => {
                    var id = context.GetRouteValue("id").ToString();
                    await context.Response.WriteAsync(id);
                });

                endpoints.MapPost("user", context => Task.CompletedTask);
            });
        }
    }
}
