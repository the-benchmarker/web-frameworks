using Simplify.Web;
using Simplify.Web.Attributes;

namespace web.Controllers
{
	[Post("/user")]
	public class UserController : Controller
	{
		public override ControllerResponse Invoke() => StatusCode(200);
	}
}