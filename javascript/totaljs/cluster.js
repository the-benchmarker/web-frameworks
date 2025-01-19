import cluster from 'node:cluster';
import { availableParallelism } from 'node:os';

const numCpus = availableParallelism();
import app from './app.mjs';

if (numCpus > 1 && cluster.isPrimary) {
  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }
} else {
  app.run({ port: 3000, release: true, watcher: false });
}
