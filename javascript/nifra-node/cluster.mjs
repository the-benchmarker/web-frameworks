import cluster from 'node:cluster';
import { availableParallelism } from 'node:os';

const numCpus = availableParallelism();

if (cluster.isPrimary) {
  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }

  function shutdown() {
    cluster.disconnect(() => process.exit(0));
  }

  process.on('SIGINT', shutdown);
  process.on('SIGTERM', shutdown);
} else {
  await import(`./${process.env.NODE_APP}`);
  console.log(`Worker ${process.pid} started`);
}
