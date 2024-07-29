using Simplify.Web;
using Simplify.Web.Attributes;

namespace web.Controllers
{
	[Post("/user")]
	public class UserController : Controller2
	{
		public ControllerResponse Invoke() => StatusCode(200);
	}
}