using Simplify.Web;

var builder = WebApplication.CreateBuilder();
builder.WebHost.ConfigureKestrel(o => o.AddServerHeader = false);
builder.Logging.ClearProviders();

var app = builder.Build();

app.UseSimplifyWeb();

app.Run();

