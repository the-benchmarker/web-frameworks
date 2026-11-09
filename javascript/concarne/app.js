// Concarne benchmark app: must answer the harness routes.
// GET / -> 200 empty, GET /user/:id -> 200 id, POST /user -> 200 empty.
var concarne = require("pkg/concarne");
var os = require("std/os");

// Size the worker pool to the host, like the node/bun cluster wrappers.
// concarne serves every worker from an isolated runtime sharing port 3000.
var workers = 1;
try {
  var n = parseInt(os.exec("nproc").output.trim(), 10);
  if (n >= 1 && n <= 64) workers = n;
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
