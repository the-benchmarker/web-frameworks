using GenHTTP.Engine.Internal;

using web;

var app = new BenchmarkHandler();

return await Host.Create()
                 .Handler(app)
                 .Port(3000)
                 .RunAsync();
