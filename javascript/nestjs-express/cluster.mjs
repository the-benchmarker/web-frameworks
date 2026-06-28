import cluster from 'node:cluster';
import { availableParallelism } from 'node:os';

/**
 * NestJS Cluster Module
 * 
 * Production-grade cluster management for NestJS benchmark server.
 * Optimized for multi-core performance with graceful shutdown.
 */

const numCpus = availableParallelism();
const NODE_ENV = process.env.NODE_ENV || 'production';

if (cluster.isPrimary) {
  // Only log cluster info in non-production
  if (NODE_ENV !== 'production') {
    console.log(`Primary ${process.pid} is running`);
    console.log(`Forking ${numCpus} workers...`);
  }

  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }

  function shutdown() {
    if (NODE_ENV !== 'production') {
      console.log('Primary received shutdown signal. Disconnecting workers...');
    }
    cluster.disconnect(() => process.exit(0));
  }

  process.on('SIGINT', shutdown);
  process.on('SIGTERM', shutdown);
} else {
  await import(`./${process.env.NODE_APP}`);
  // Only log worker startup in non-production
  if (NODE_ENV !== 'production') {
    console.log(`Worker ${process.pid} started`);
  }
}
