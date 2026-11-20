var builder = WebApplication.CreateBuilder();
builder.WebHost.ConfigureKestrel(o => o.AddServerHeader = false);
builder.Logging.ClearProviders();

var app = builder.Build();

app.MapGet("/", () => { });

app.MapGet("user/{id}", (string id) => id);

app.MapPost("user", () => { });

app.Run();
