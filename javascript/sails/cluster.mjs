import cluster from 'node:cluster';
import { availableParallelism } from 'node:os';
import { exec } from "child_process"

const numCpus = availableParallelism();

if (numCpus > 1 && cluster.isPrimary) {
  for (let i = 0; i < numCpus; i++) {
    cluster.fork();
  }
} else {
  exec('sails lift --prod')
}
