import { Router, BunicornApp } from "@bunicorn/server";

const app = new BunicornApp();
const router = new Router();
app.addRoutes([
  router.get("/", (ctx) => ctx.ok()),
  router.get("/user/:id", (ctx) => ctx.raw(ctx.params.id)),
  router.post("/user", (ctx) => ctx.ok()),
]);

Bun.serve({
  fetch(req) {
    return app.handleRequest(req);
  },
  port: 3000, reusePort: true
});
