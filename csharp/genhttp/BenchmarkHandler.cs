using GenHTTP.Api.Content;
using GenHTTP.Api.Protocol;

using Strings = GenHTTP.Modules.IO.Strings;

namespace web;

/// <summary>
/// Production-grade benchmark handler for GenHTTP
/// Implements security, performance, and maintainability best practices
/// </summary>
internal sealed class BenchmarkHandler : IHandler
{
    private static readonly FlexibleContentType _TextType = FlexibleContentType.Get(ContentType.TextPlain);
    private static readonly Strings.StringContent _EmptyContent = new("");

    /// <summary>
    /// Prepare handler for requests
    /// </summary>
    /// <returns>Completed value task</returns>
    public ValueTask PrepareAsync() => new();

    /// <summary>
    /// Handle incoming HTTP requests
    /// </summary>
    /// <param name="request">Incoming request</param>
    /// <returns>Response value task</returns>
    public ValueTask<IResponse> HandleAsync(IRequest request)
    {
        // Security: Add security headers to all responses
        var responseBuilder = request.Respond()
            .Type(_TextType)
            .Header("X-Content-Type-Options", "nosniff")
            .Header("X-Frame-Options", "DENY")
            .Header("X-XSS-Protection", "1; mode=block")
            .Header("Referrer-Policy", "strict-origin-when-cross-origin")
            .Header("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
            .Header("Cache-Control", "no-cache,no-store,must-revalidate");

        var target = request.Target;
        IResponse response;

        // Handle root path (GET /)
        if (target.Ended)
        {
            response = responseBuilder.Content(_EmptyContent).Build();
        }
        // Handle /user path
        else if (target.Current.Original == "user")
        {
            target.Advance();

            // GET /user - create user (POST would have different method)
            if (target.Ended)
            {
                response = responseBuilder.Content(_EmptyContent).Build();
            }
            // GET /user/{id} - get user by ID
            else
            {
                var id = target.Current.Original;
                
                // Security: Validate input
                if (string.IsNullOrWhiteSpace(id))
                {
                    response = responseBuilder
                        .Status(400)
                        .Content(new Strings.StringContent("Bad Request"))
                        .Build();
                }
                else
                {
                    response = responseBuilder.Content(new Strings.StringContent(id)).Build();
                }
            }
        }
        else
        {
            // 404 Not Found for unknown paths
            response = responseBuilder
                .Status(404)
                .Content(new Strings.StringContent("Not Found"))
                .Build();
        }

        return new(response);
    }

    /// <summary>
    /// Helper method to create empty response with security headers
    /// </summary>
    /// <param name="request">Original request</param>
    /// <param name="content">Response content (optional)</param>
    /// <returns>Configured response</returns>
    private static IResponse GetEmptyResponse(IRequest request, Strings.StringContent content = null)
    {
        return request.Respond()
                      .Type(_TextType)
                      .Content(content ?? _EmptyContent)
                      .Header("X-Content-Type-Options", "nosniff")
                      .Header("X-Frame-Options", "DENY")
                      .Header("Cache-Control", "no-cache,no-store,must-revalidate")
                      .Build();
    }

}