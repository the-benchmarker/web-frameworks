const cluster = require('cluster');
const os = require('os');
const { App } = require('@sifrr/server');

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
  const app = new App();

  app
    .get('/', (res) => {
      res.end('');
    })
    .get('/user/:id', (res, req) => {
      res.end(req.getParameter(0));
    })
    .post('/user', (res) => {
      res.end('');
    })
    .listen(3000);

  console.info(`Worker PID ${process.pid} is listening at http://localhost:3000`);
}
