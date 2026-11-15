const server = Bun.serve({
  hostname: '0.0.0.0',
  port: 3000,
  reusePort: true,
  development: false,
  routes: {
    '/': () => new Response(null, { status: 204 }),
    '/user': () => new Response(null, { status: 204 }),
    '/user/:id': ({ params }) => new Response(params.id, { status: 200 }),
  },
});
