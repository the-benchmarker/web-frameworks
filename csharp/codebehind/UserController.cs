using CodeBehind;

public partial class UserController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        if (Section.Count() > 0)
            Write(Section.GetValue(0)); // path: /user/id
        else
            Write("256"); // path: /user
    }
}
