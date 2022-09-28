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
                       o.LogToConsole = false;
                       o.ManageApiEnabled = false;
                       o.WriteLog = false;
                       o.Port = 3000;
                       o.SetDebug();
                       o.LogLevel = BeetleX.EventArgs.LogType.Off;
                   },
                   typeof(Program).Assembly);
               });
            builder.Build().Run();
        }
        [Get(Route = "/")]
        public object index()
        {
            return new TextResult(null);
        }
        [Get(Route = "/user/{id}")]
        public object user(string id)
        {
            return new TextResult(id);
        }
        [Post(Route = "/user")]
        public object create()
        {
            return new TextResult(null);
        }
    }
}
