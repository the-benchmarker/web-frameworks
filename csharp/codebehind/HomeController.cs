using CodeBehind;

public partial class DefaultController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        Write(""); // path: /
    }
}
