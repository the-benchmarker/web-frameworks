const concarne = require("pkg/concarne");
const os = require("std/os");
const app = concarne();

app.get("/", function (req, res) {
  res.send("");
});

app.get("/user/:id", function (req, res) {
  res.send(req.params.id);
});

app.post("/user", function (req, res) {
  res.send("");
});

app.listen(3000, {workers: Math.min(os.cpuCount(), 64)}, (info) => {
  if (!process.isWorker) {
    console.log("workers on http://" + info.host + ":" + info.port + " x" + info.workers);
  }
});