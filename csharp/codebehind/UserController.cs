using CodeBehind;

public partial class user : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {   
        Write(Segment.GetValue(0)); // path: /user/{id}
    }
}

