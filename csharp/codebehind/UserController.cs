using CodeBehind;

public partial class UserController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        if (Section.Count() > 0)
            context.Response.WriteAsync(Section.GetValue(0)); // path: /user/id
        else
            Write(""); // path: /user
    }
}
