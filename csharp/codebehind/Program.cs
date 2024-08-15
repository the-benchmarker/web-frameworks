var builder = WebApplication.CreateBuilder(args);

var app = builder.Build();

SetCodeBehind.CodeBehindCompiler.Initialization();

app.UseCodeBehindRoute();

app.Run();
