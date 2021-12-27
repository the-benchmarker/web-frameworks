namespace web
{
    using Carter;
    using Carter.Request;
    using Microsoft.AspNetCore.Http;
    using System.Threading.Tasks;

    public class UserModule : CarterModule
    {
        public UserModule()
        {
            Get("/user/{id}", async(req, res) =>
            {
                var id = req.RouteValues.As<string>("id");
                await res.WriteAsync(id);
            });

            Post("/user", (req, res) => Task.CompletedTask);
        }
    }
}
