var cluster = require('cluster');
var os = require('os');
var express = require('express');

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
  var app = express();
  app.set('etag', false);

  app.get('/', function (req, res) {
    res.send('');
  });

  app.get('/user/:id', function (req, res) {
    res.send(req.params.id);
  });

  app.post('/user', function (req, res) {
    res.send('');
  });

  app.listen(3000, function (err, address) {
    if (err) {
      app.log.error(err);
      process.exit(1);
    }
    console.info(`Worker PID ${process.pid} is listening at http://localhost:3000`);
  });
}
