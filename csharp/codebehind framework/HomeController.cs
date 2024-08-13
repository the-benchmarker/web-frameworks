using CodeBehind;

public partial class HomeController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        Write(""); // path: /
    }
}