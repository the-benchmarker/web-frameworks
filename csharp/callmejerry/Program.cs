using Verstappen;

var app = new Max();

app.Get("/", static _ => { });

app.Get("/user/:id", static ctx => ctx.Write(ctx.RouteValue(0)));

app.Post("/user", static _ => { });

app.Run(3000);
