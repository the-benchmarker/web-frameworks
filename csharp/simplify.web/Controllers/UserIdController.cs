using Simplify.Web;
using Simplify.Web.Attributes;

namespace web.Controllers
{
	[Get("/user/{id}")]
	public class UserIdController : Controller2
	{
		public ControllerResponse Invoke(string id) => Content(id);
	}
}
