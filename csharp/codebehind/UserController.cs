using CodeBehind;

public partial class UserController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        if (Section.Count() > 0)
            Write("0"); // path: /user/id
        else
            Write(""); // path: /user
    }
}
