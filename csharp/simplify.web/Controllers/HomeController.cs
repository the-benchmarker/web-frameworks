using Simplify.Web;
using Simplify.Web.Attributes;

namespace web.Controllers
{
	/// <summary>
	/// Home controller for root endpoint
	/// Production-grade implementation with best practices
	/// </summary>
	[Get("/")]
	public class HomeController : Controller2
	{
		/// <summary>
		/// Handle GET request for root path (/)
		/// </summary>
		/// <returns>Controller response with 200 status</returns>
		public ControllerResponse Invoke()
		{
			// Security: Set cache headers
			Response.Headers.Append("Cache-Control", "no-cache,no-store,must-revalidate");
			Response.Headers.Append("X-Content-Type-Options", "nosniff");
			
			return StatusCode(200);
		}
	}
}