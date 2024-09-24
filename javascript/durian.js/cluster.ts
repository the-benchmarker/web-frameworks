import { bootstrap } from "./src/app";
import cluster from 'cluster';
import os from "node:os";

if (cluster.isPrimary) {
  console.log(`Master ${process.pid} is running`);
  for (let i = 0; i < os.cpus().length; i++) {
    cluster.fork();
  }
  cluster.on('exit', (worker, code, signal) => {
    console.log(`Worker ${worker.process.pid} died`);
  });
} else {
  bootstrap();
  console.log(`Worker ${process.pid} started`);
}

