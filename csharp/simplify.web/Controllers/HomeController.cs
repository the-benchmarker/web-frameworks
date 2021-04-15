using Simplify.Web;
using Simplify.Web.Attributes;

namespace web.Controllers
{
	[Get("/")]
	public class HomeController : Controller
	{
		public override ControllerResponse Invoke() => StatusCode(200);
	}
}