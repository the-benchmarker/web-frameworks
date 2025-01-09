namespace web
{
    using System.Threading.Tasks;
    using Carter;
    using Microsoft.AspNetCore.Builder;
    using Microsoft.AspNetCore.Routing;

    public class HomeModule : CarterModule
    {
        public override void AddRoutes(IEndpointRouteBuilder app)
        {
            app.MapGet("/", () => Task.CompletedTask);
        }
    }
}
