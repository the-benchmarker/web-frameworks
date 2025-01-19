import cluster from 'node:cluster';
import { availableParallelism } from 'node:os';

const numCpus = availableParallelism();
import { serve } from "@hono/node-server";
import app from './app.js';

if (numCpus > 1 && cluster.isPrimary) {
  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }
} else {
  serve(app);
}
