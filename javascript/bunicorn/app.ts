import { RouteBuilder, BunicornApp } from "@bunicorn/server";

const app = new BunicornApp({ basePath: "/" });
const rb = new RouteBuilder();
app.addRoutes([
  rb.get("/", (ctx) => ctx.ok()),
  rb.get("/user/:id", (ctx) => ctx.raw(ctx.params.id)),
  rb.post("/user", (ctx) => ctx.ok()),
]);

app.listen(3000);