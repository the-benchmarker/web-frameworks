import { Elysia } from "elysia";

const app = new Elysia();

app.get("/", () => "");

app.get("/user/:id", (req) => req.params.id);

app.post("/user", () => "");

Bun.serve({
  fetch: app.fetch,
  reusePort: true,
  port: 3000
})
