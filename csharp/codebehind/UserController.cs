using CodeBehind;

public partial class user : CodeBehindController
{
    public async Task PageLoad(HttpContext context)
    {   
        Write(Segment.GetValue(0)); // path: /user/{id}
    }
}


