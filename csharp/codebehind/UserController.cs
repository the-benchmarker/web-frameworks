using CodeBehind;

public partial class user : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {   
        Write(Section.GetValue(0)); // path: /user/{id}
    }
}
