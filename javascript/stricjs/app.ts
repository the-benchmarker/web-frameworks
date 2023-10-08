import { Router, macro } from '@stricjs/router';

const app = new Router();

app.get("/", macro(() => new Response()));

// @ts-ignore A bit hacky here
app.get("/user/:id", r => new Response(r.params.id));

app.post("/user", macro(() => new Response()));

Bun.serve({
  fetch: app.fetch,
  reusePort: true,
  port: 3000
})
