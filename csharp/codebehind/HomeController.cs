using CodeBehind;

public partial class DefaultController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        IgnoreViewAndModel = true;
        Write(""); // path: /
    }
}
