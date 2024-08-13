using CodeBehind;

public partial class HomeController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        if (context.Request.Method == "GET")
            Write(""); // path: /
    }
}
