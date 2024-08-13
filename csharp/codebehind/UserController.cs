using CodeBehind;

public partial class UserController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        if (Section.Count() > 0)
        {
            if (context.Request.Method == "GET" || string.IsNullOrEmpty(context.Request.Method))
                Write(Section.GetValue(0)); // path: /user/id
        }
        else
            if (context.Request.Method == "POST")
                Write(""); // path: /user
    }
}
