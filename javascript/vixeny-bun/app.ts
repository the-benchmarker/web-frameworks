const app = await import("./server.ts");

Bun.serve({
  fetch: await app.compose(),
});
