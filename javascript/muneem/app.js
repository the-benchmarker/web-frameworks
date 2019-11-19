const app = require("muneem")();

app.get("/", function(req, res) {
  res.end(200);
});

app.get("/user/:id", function(req, res) {
  res.write(req.params.id);
  res.end();
});

app.post("/user", function(req, res) {
  res.end(200);
});

app.start(3000);
