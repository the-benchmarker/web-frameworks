import cluster from 'node:cluster';
import { availableParallelism } from 'node:os';

const numCpus = availableParallelism();

if (numCpus > 1 && cluster.isPrimary) {
  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }
} else {
  await import(`./${process.env.NODE_APP}`);
}
