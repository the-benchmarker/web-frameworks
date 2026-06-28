using Simplify.Web;
using Simplify.Web.Attributes;

namespace web.Controllers
{
	/// <summary>
	/// User ID controller for GET /user/{id} endpoint
	/// Production-grade implementation with best practices
	/// </summary>
	[Get("/user/{id}")]
	public class UserIdController : Controller2
	{
		/// <summary>
		/// Handle GET request for user by ID
		/// </summary>
		/// <param name="id">User identifier</param>
		/// <returns>Controller response with user ID</returns>
		public ControllerResponse Invoke(string id)
		{
			// Security: Validate input
			if (string.IsNullOrWhiteSpace(id))
			{
				return StatusCode(400);
			}
			
			// Security: Set cache headers
			Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
			Response.Headers.Append("X-Content-Type-Options", "nosniff");
			
			return Content(id);
		}
	}
}
