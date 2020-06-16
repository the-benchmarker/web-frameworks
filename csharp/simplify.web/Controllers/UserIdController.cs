using Simplify.Web;
using Simplify.Web.Attributes;

namespace web.Controllers
{
	[Get("/user/{id}")]
	public class UserIdController : Controller
	{
		public override ControllerResponse Invoke()
		{
			var content = this.Context.Route.Substring(6);
			return Content(content);
		}
	}
}