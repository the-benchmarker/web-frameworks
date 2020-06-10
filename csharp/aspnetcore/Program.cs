using System.IO;
using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Logging;

namespace aspnetcore
{
    public class Program
    {
        public static void Main(string[] args) =>
            WebHost.CreateDefaultBuilder(args)
                .ConfigureLogging(config => config.ClearProviders())
                .UseStartup<Startup>()
                .Build()
                .Run();
    }
}
