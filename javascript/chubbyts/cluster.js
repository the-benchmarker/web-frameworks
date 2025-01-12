import cluster from 'node:cluster';
import { availableParallelism } from 'node:os';
import server from './app.js';

const numCpus = availableParallelism();

if (numCpus > 1 && cluster.isPrimary) {
  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }
} else {
  server.listen(3000, '0.0.0.0');
}
