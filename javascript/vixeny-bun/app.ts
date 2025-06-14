import app from "./server.ts"

Bun.serve({
  fetch: await app.compose(),
  reusePort: true
});
