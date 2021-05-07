using BeetleX.FastHttpApi;
using BeetleX.FastHttpApi.Hosting;
using Microsoft.Extensions.Hosting;
using System;

namespace web
{
    [Controller]
    public class Program
    {
        static void Main(string[] args)
        {
            var builder = new HostBuilder()
               .ConfigureServices((hostContext, services) =>
               {
                   services.UseBeetlexHttp(o =>
                   {
                       o.LogToConsole = true;
                       o.ManageApiEnabled = false;
                       o.WriteLog = false;
                       o.Port = 80;
                       o.SetDebug();
                       o.LogLevel = BeetleX.EventArgs.LogType.Warring;
                   },
                   typeof(Program).Assembly);
               });
            builder.Build().Run();
        }
        [Get(Route = "{id}")]
        public object user(string id)
        {
            return new TextResult(id);
        }
    }
}
