import { RouteBuilder, BunicornApp } from "@bunicorn/server";

const app = new BunicornApp({ basePath: "/" });
const rb = new RouteBuilder();
app.addRoutes([
  rb.get("/", (ctx) => ctx.ok()),
  rb.get("/user/:id", (ctx) => ctx.raw(ctx.params.id)),
  rb.post("/user", (ctx) => ctx.ok()),
]);

// @ts-expect-error
Bun.serve({
  fetch(req: Request) {
    return app.handleRequest(req);
  },
  port: 3000,
});
