using FastEndpoints;

var builder = WebApplication.CreateBuilder();
builder.Logging.ClearProviders(); //logging results in a ~95% perf drop
builder.Services.AddFastEndpoints();

var app = builder.Build();
app.UseFastEndpoints();
app.Run();

public class Home : EndpointWithoutRequest
{
    public override void Configure()
    {
        Get("/");
        AllowAnonymous();
    }

    public override Task HandleAsync(CancellationToken __)
    {
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        return HttpContext.Response.WriteAsync("");
    }
}

public class UserById : EndpointWithoutRequest
{
    public override void Configure()
    {
        Get("/user/{id}");
        AllowAnonymous();
    }

    public override Task HandleAsync(CancellationToken __)
    {
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        return HttpContext.Response.WriteAsync(
            HttpContext.Request.RouteValues["id"].ToString());
    }
}

public class User : EndpointWithoutRequest
{
    public override void Configure()
    {
        Post("/user");
        AllowAnonymous();
        Description(x => x.Accepts<object>("*/*"));
    }

    public override Task HandleAsync(CancellationToken __)
    {
        HttpContext.Response.StatusCode = StatusCodes.Status200OK;
        return HttpContext.Response.WriteAsync("");
    }
}
