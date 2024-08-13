var builder = WebApplication.CreateBuilder(args);

var app = builder.Build();

SetCodeBehind.CodeBehindCompiler.Initialization(); // Set Initialization(true) Before Publish Project

app.Run(async context =>
{
    CodeBehind.CodeBehindExecute execute = new CodeBehind.CodeBehindExecute();
    string Result = execute.Run(context);

    if (execute.FoundPage)
        await context.Response.WriteAsync(Result);
    else
        context.Response.StatusCode = 404;
});

app.Run();