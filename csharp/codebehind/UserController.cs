using CodeBehind;

/// <summary>
/// User controller for /user/{id} endpoint
/// Production-grade implementation with best practices
/// </summary>
public partial class UserController : CodeBehindController
{
    /// <summary>
    /// Handles GET requests to /user/{id} path
    /// </summary>
    /// <param name="context">HTTP context</param>
    public void PageLoad(HttpContext context)
    {
        // Security: Validate and sanitize input
        var id = Segment.GetValue(0);
        
        if (string.IsNullOrWhiteSpace(id))
        {
            context.Response.StatusCode = 400;
            context.Response.ContentType = "text/plain";
            context.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
            Write("Bad Request");
            return;
        }
        
        // Security: Set proper headers
        context.Response.ContentType = "text/plain";
        context.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        context.Response.Headers.Append("X-Content-Type-Options", "nosniff");
        
        // Write user ID for benchmarking
        Write(id);
    }
}
