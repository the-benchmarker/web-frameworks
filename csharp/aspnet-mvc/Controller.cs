using Microsoft.AspNetCore.Mvc;

namespace web;

/// <summary>
/// Main API Controller for benchmarking endpoints
/// Production-grade implementation with best practices
/// </summary>
[ApiController]
[Route("[controller]")]
[Produces("text/plain")]
public class BenchmarkController : ControllerBase
{
    /// <summary>
    /// Root endpoint - GET /
    /// Returns empty response for benchmarking
    /// </summary>
    /// <returns>Empty response</returns>
    [HttpGet("/")]
    [ProducesResponseType(typeof(string), 200)]
    public IActionResult GetHome()
    {
        // Security: Set cache headers for benchmarking responses
        Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        
        return Ok("");
    }

    /// <summary>
    /// Get user by ID - GET /user/{id}
    /// Returns the ID for benchmarking
    /// </summary>
    /// <param name="id">User identifier</param>
    /// <returns>User ID as response</returns>
    [HttpGet("/user/{id}")]
    [ProducesResponseType(typeof(string), 200)]
    public IActionResult GetUserById(string id)
    {
        // Security: Validate input
        if (string.IsNullOrWhiteSpace(id))
        {
            return BadRequest();
        }
        
        Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        return Ok(id);
    }

    /// <summary>
    /// Create user - POST /user
    /// Returns empty response for benchmarking
    /// </summary>
    /// <returns>Empty response</returns>
    [HttpPost("/user")]
    [ProducesResponseType(typeof(string), 200)]
    public IActionResult CreateUser()
    {
        Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
        return Ok("");
    }
}
