namespace web
{
    using System.Threading.Tasks;
    using Carter;
    using Microsoft.AspNetCore.Builder;
    using Microsoft.AspNetCore.Mvc;
    using Microsoft.AspNetCore.Routing;

    public class UserModule : CarterModule
    {
        public override void AddRoutes(IEndpointRouteBuilder app)
        {
            app.MapGet("/user/{id}", async ([FromRoute] string id) =>
            {
                return id;
            });

            app.MapPost("/user", () => Task.CompletedTask);
        }
    }
}
