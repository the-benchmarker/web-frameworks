import cluster from 'node:cluster';
import { availableParallelism } from 'node:os';
import app from './dist/main.js';

const numCpus = availableParallelism();

if (numCpus > 1 && cluster.isPrimary) {
  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }
} else {
  app.listen(3000, '0.0.0.0');
}
