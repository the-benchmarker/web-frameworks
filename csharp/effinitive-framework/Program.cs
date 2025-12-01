using EffinitiveFramework.Core;

var app = EffinitiveApp
    .Create()
    .UsePort(3000)
    .MapEndpoints()
    .Build();

await app.RunAsync();

public class RootEndpoint : NoRequestEndpointBase<string>
{
    protected override string Method => "GET";
    protected override string Route => "/";
    protected override string ContentType => "text/plain";

    public override ValueTask<string> HandleAsync(CancellationToken cancellationToken = default)
    {
        return ValueTask.FromResult(string.Empty);
    }
}

public class GetUserEndpoint : NoRequestEndpointBase<string>
{
     protected override string Method => "GET";
    protected override string Route => "/user/{id}";
    protected override string ContentType => "text/plain";

    public override ValueTask<string> HandleAsync(CancellationToken cancellationToken = default)
    {
        return ValueTask.FromResult(HttpContext?.RouteValues?["id"] ?? string.Empty);
    }
}

public class CreateUserEndpoint : NoRequestEndpointBase<string>
{
    protected override string Method => "POST";
    protected override string Route => "/user";
    protected override string ContentType => "text/plain";

    public override ValueTask<string> HandleAsync(CancellationToken cancellationToken = default)
    {
        return ValueTask.FromResult(string.Empty);
    }
}
