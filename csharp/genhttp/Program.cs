using GenHTTP.Engine.Internal;

using Microsoft.Extensions.Logging.Abstractions;

using web;

var app = new BenchmarkHandler();

return await Host.Create()
                 .Handler(app)
                 .Logging(NullLoggerFactory.Instance, false)
                 .Port(3000)
                 .RunAsync();
