using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

namespace aspnetcore
{
    public class Program
    {
        public static void Main(string[] args) =>
            Host.CreateDefaultBuilder(args)
                .ConfigureWebHostDefaults(webBuilder =>
                {
                    webBuilder.ConfigureKestrel(c => c.AddServerHeader = false);
                    webBuilder.ConfigureLogging(config => config.ClearProviders());
                    webBuilder.UseStartup<Startup>();
                })
                .Build()
                .Run();
    }
}
