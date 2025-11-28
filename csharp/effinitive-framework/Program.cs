using EffinitiveFramework.Core;

var app = EffinitiveApp
    .Create()
    .UsePort(3000)
    .MapEndpoints()
    .Build();

await app.RunAsync();

public class RootEndpoint : AsyncEndpointBase<EmptyRequest, EmptyResponse>
{
    protected override string Method => "GET";
    protected override string Route => "/";

    public override Task<EmptyResponse> HandleAsync(
        EmptyRequest request,
        CancellationToken cancellationToken = default)
    {
        return Task.FromResult(new EmptyResponse());
    }
}

public class GetUserEndpoint : AsyncEndpointBase<UserIdRequest, string>
{
    protected override string Method => "GET";
    protected override string Route => "/user/{id}";

    public override Task<string> HandleAsync(
        UserIdRequest request,
        CancellationToken cancellationToken = default)
    {
        return Task.FromResult(request.Id);
    }
}

public class CreateUserEndpoint : AsyncEndpointBase<EmptyRequest, EmptyResponse>
{
    protected override string Method => "POST";
    protected override string Route => "/user";

    public override Task<EmptyResponse> HandleAsync(
        EmptyRequest request,
        CancellationToken cancellationToken = default)
    {
        return Task.FromResult(new EmptyResponse());
    }
}

public class EmptyRequest { }
public class EmptyResponse { }
public class UserIdRequest
{
    public string Id { get; set; } = string.Empty;
}
