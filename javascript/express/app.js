const express = require("express");
const cluster = require("cluster");
const os = require("os")

const app = express();
app.set("etag", false);

app.get("/", function (req, res) {
  res.send("");
});

app.get("/user/:id", function (req, res) {
  res.send(req.params.id);
});

app.post("/user", function (req, res) {
  res.send("");
});

const start = async () => {
    try {
        await app.listen(3000);
    } catch (err) {
        process.exit(1);
    }
}

const clusterWorkerSize = os.cpus().length;

if (cluster.isMaster) {
  for (let i=0 ; i < clusterWorkerSize ; i++) {
     cluster.fork();
  }

  cluster.on("exit", function(worker) {
    console.log("Worker", worker.id, " has exited.")
    cluster.fork();
  })
} else {
  start();
}
