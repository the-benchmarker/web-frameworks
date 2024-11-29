const cluster = require('cluster');
const os = require('os');
const fastify = require('fastify');

if (cluster.isPrimary) {
  const numCPUs = os.cpus().length;
  console.log(`Primary PID ${process.pid} is running`);

  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    // The worker processes are spawned using the child_process.fork() method,
    // so that they can communicate with the parent via IPC and pass server handles back and forth.
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
    console.log(`Worker PID ${worker.process.pid} died`);
    if (signal) {
      console.log(`worker was killed by signal: ${signal}`);
    } else if (code !== 0) {
      console.log(`worker exited with error code: ${code}`);
    } else {
      console.log('worker success!');
    }
    cluster.fork(); // Restart the worker
  });
} else {
  // Workers can share any TCP connection
  const app = fastify();

  app.get('/', function (request, reply) {
    reply.send();
  });

  app.get('/user/:id', function (request, reply) {
    reply.send(request.params.id);
  });

  app.post('/user', function (request, reply) {
    reply.send();
  });

  // Running Node.js will now share port 3000 between the workers:
  app.listen({ port: 3000, host: '0.0.0.0' }, function (err, address) {
    if (err) {
      app.log.error(err);
      process.exit(1);
    }
    console.info(`Worker PID ${process.pid} is listening at ${address}`);
  });
}
