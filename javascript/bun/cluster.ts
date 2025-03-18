import cluster, { type Worker } from 'node:cluster';

function startServer() {
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

  console.debug(`Worker PID: ${process.pid} listening on ${server.url}`);
}

function forkWorkers() {
  console.log(`Primary PID: ${process.pid}`);
  for (let i = 0; i < navigator.hardwareConcurrency; i++) cluster.fork();

  cluster.on('exit', (worker, code, signal) => {
    console.error(`Worker PID: ${worker.process.pid} died with code ${code} and signal ${signal}`);
  });
}

if (cluster.isPrimary) forkWorkers();
else startServer();
