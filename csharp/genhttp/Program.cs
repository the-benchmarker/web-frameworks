using GenHTTP.Engine.Kestrel;
using GenHTTP.Modules.Functional;

var empty = "";

var app = Inline.Create()
                .Get(() => empty)
                .Get("/user/:id", (string id) => id)
                .Post("/user", () => empty);

return await Host.Create()
                 .Handler(app)
                 .Port(3000)
                 .RunAsync();
