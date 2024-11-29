const Hapi = require('@hapi/hapi');
const cluster = require('cluster');
const os = require('os');

if (cluster.isPrimary) {
  const numCPUs = os.cpus().length;

  console.log(`Master ${process.pid} is running`);

  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
    console.log(`Worker ${worker.process.pid} died`);
    cluster.fork(); // Restart the worker
  });
} else {
  // Workers can share any TCP connection
  // In this case it is an HTTP server

  // Create a server with a host and port
  const server = Hapi.server({
    host: '0.0.0.0',
    port: 3000,
  });

  // Add the route
  server.route({
    method: 'GET',
    path: '/',
    handler: function (req, handler) {
      return handler.response('').header('Content-Length', '0');
    },
  });

  server.route({
    method: 'GET',
    path: '/user/{id}',
    handler: function (req, handler) {
      return req.params.id;
    },
  });

  server.route({
    method: 'POST',
    path: '/user',
    handler: function (req, handler) {
      return handler.response('').header('Content-Length', '0');
    },
  });

  // Start the server
  async function start() {
    try {
      await server.start();
    } catch (err) {
      console.log(err);
      process.exit(1);
    }

    console.log(`Worker ${process.pid} running at:`, server.info.uri);
  }

  start();
}
