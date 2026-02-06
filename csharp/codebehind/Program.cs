var builder = WebApplication.CreateBuilder(args);
builder.WebHost.ConfigureKestrel(c => c.AddServerHeader = false);
builder.Logging.ClearProviders();

builder.Services.AddCodeBehind();

var app = builder.Build();

app.UseCodeBehindRouteAsync();

app.Run();

