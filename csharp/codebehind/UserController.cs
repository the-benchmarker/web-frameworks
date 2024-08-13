using CodeBehind;

public partial class UserController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        if (Section.Count() > 0)
        {
            Write(Section.GetValue(1)); // path: /user/id
        }
        else
            if (context.Request.Method == "POST")
                Write(""); // path: /user
    }
}
