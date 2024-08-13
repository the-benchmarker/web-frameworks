using CodeBehind;

public partial class UserController : CodeBehindController
{
    public void CodeBehindConstructor()
    {
        if (Section.Count() > 0)
            Write(Section.GetValue(0)); // path: /user/id
        else
            Write(""); // path: /user
    }
}
