const { Router } = require("@routejs/router");
const http = require("http");

const app = new Router();

app.get("/", function (req, res) {
  res.end("");
});

app.get("/user/:id", function (req, res) {
  res.end(req.params.id);
});

app.post("/user", function (req, res) {
  res.end("");
});

const server = http.createServer(app.handler());
server.listen(3000);
