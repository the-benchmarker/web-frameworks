using CodeBehind;

public partial class UserController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        if (Section.Count() > 0)
        {
            if (context.Request.Method == "GET")
                Write(Section.GetValue(0)); // path: /user/id
        }
        else
            if (context.Request.Method == "POST")
                Write(""); // path: /user
    }
}
