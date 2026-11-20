using Simplify.Web;
using Simplify.Web.Attributes;

namespace web.Controllers
{
	[Get("/")]
	public class HomeController : Controller2
	{
		public ControllerResponse Invoke() => StatusCode(200);
	}
}