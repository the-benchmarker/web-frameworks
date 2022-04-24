public class Middleware
{
    private readonly RequestDelegate _next;

    public Middleware(RequestDelegate next)
    {
        _next = next;
    }

    public Task InvokeAsync(HttpContext httpContext)
    {
        Func<Task>? result = httpContext.Request.Path.Value switch
        {
            "/" => () => Task.CompletedTask,
            "/user" => () => Task.CompletedTask,
            _ => null
        };

        if (result is not null)
        {
            return result();
        }

        string[] splits = httpContext.Request.Path.Value?.Split(
            '/', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)!;

        if (splits[0] is "user" && splits.Length == 2)
        {
            return httpContext.Response.WriteAsync(splits[1]);
        }

        return _next(httpContext);
    }
}