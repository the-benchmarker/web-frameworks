using System.IO;
using Microsoft.AspNetCore.Hosting;

namespace aspnetcore
{
  public class Program
    {
        public static void Main(string[] args)
        {
            var host = new WebHostBuilder()
                .UseKestrel()
                .UseStartup<Startup>()
                .UseUrls("http://0.0.0.0:8009")
                .Build();

            host.Run();
        }
    }
}
