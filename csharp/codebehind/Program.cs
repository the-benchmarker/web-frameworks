var builder = WebApplication.CreateBuilder(args);
builder.WebHost.ConfigureKestrel(c => c.AddServerHeader = false);
builder.Logging.ClearProviders();

var app = builder.Build();

SetCodeBehind.CodeBehindCompiler.Initialization(true);

app.UseCodeBehindRoute();

app.Run();
