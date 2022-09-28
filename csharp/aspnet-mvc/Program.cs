var builder = WebApplication.CreateBuilder();
builder.Logging.ClearProviders();
builder.Services.AddControllers();

var app = builder.Build();
app.UseAuthorization();
app.MapControllers();
app.Run();
