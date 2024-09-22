import { Router, BunicornApp } from "@bunicorn/server";

const app = new BunicornApp();
const rb = new Router();
app.addRoutes([
  rb.get("/", (ctx) => ctx.ok()),
  rb.get("/user/:id", (ctx) => ctx.raw(ctx.params.id)),
  rb.post("/user", (ctx) => ctx.ok()),
]);

app.serve({
  port: 3000,
	reusePort: true
})
