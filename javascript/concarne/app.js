// Concarne benchmark app: must answer the harness routes.
// GET / -> 200 empty, GET /user/:id -> 200 id, POST /user -> 200 empty.
var concarne = require("pkg/concarne");
var os = require("std/os");

// Use every CPU core, like the node/bun cluster wrappers: concarne
// serves each worker from an isolated runtime sharing port 3000.
// std/http supports 1..64 workers, so clamp large hosts to 64.
var workers = 1;
try {
  var n = parseInt(os.exec("nproc").output.trim(), 10);
  if (n >= 1) workers = Math.min(n, 64);
} catch (e) {}

var app = concarne();

app.get("/", function (req, res) {
  res.send("");
});

app.get("/user/:id", function (req, res) {
  res.send(req.params.id);
});

app.post("/user", function (req, res) {
  res.send("");
});

app.listen(3000, "0.0.0.0", {workers: workers});
