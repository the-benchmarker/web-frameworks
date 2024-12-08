import cluster from "node:cluster";
import os from "node:os";

const cpus = os.availableParallelism();

// Node Cluster for npm packages
if (cluster.isPrimary) {
  for (let i = 0; i < cpus; i++) {
    cluster.fork();
  }
} else {
  await import("./app.ts");
}