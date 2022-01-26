using FastEndpoints;

var builder = WebApplication.CreateBuilder();
builder.Services.AddFastEndpoints();

var app = builder.Build();
app.UseFastEndpoints();
app.Run();

public class Home : Endpoint<object, object>
{
    public override void Configure()
    {
        Get("/");
        AllowAnonymous();
    }

    public override Task HandleAsync(object _, CancellationToken __)
    {
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        return HttpContext.Response.WriteAsync("");
    }
}

public class UserById : Endpoint<object, object>
{
    public override void Configure()
    {
        Get("/user/{id}");
        AllowAnonymous();
    }

    public override Task HandleAsync(object _, CancellationToken __)
    {
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        return HttpContext.Response.WriteAsync(
            HttpContext.Request.RouteValues["id"].ToString());
    }
}

public class User : Endpoint<object, object>
{
    public override void Configure()
    {
        Post("/user");
        AllowAnonymous();
        Describe(x => x.Accepts<object>("*/*"));
    }

    public override Task HandleAsync(object _, CancellationToken __)
    {
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        return HttpContext.Response.WriteAsync("");
    }
}