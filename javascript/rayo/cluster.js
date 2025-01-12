import cluster from 'node:cluster';
import { availableParallelism } from 'node:os';
import app from './app.js';

const numCpus = availableParallelism();

if (numCpus > 1 && cluster.isPrimary) {
  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }
} else {
  app.start()
}
