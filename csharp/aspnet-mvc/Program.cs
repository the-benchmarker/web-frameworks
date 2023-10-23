var builder = WebApplication.CreateBuilder();
builder.WebHost.ConfigureKestrel(o => o.AddServerHeader = false);
builder.Logging.ClearProviders();
builder.Services.AddControllers();

var app = builder.Build();
app.MapControllers();
app.Run();
