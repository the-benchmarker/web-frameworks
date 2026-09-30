var builder = WebApplication.CreateBuilder();

builder.WebHost.ConfigureKestrel(c => c.AddServerHeader = false);
builder.Logging.ClearProviders();

builder.Services.AddCodeBehind();

var app = builder.Build();

app.UseCodeBehindRoute();

app.Run();
