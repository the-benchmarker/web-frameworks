const concarne = require("pkg/concarne");
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

app.listen(3000);