using CodeBehind;

/// <summary>
/// Default controller for root endpoint
/// Production-grade implementation with best practices
/// </summary>
public partial class DefaultController : CodeBehindController
{
    /// <summary>
    /// Handles GET requests to root path (/)
    /// </summary>
    /// <param name="context">HTTP context</param>
    public void PageLoad(HttpContext context)
    {
        // Security: Set proper headers
        context.Response.ContentType = "text/plain";
        context.Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        context.Response.Headers.Append("X-Content-Type-Options", "nosniff");
        
        // Write empty response for benchmarking
        Write("");
    }
}

