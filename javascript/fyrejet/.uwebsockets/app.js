var fyrejet = require("fyrejet");
var app = fyrejet({
  prioRequestsProcessing: false,
  server: fyrejet.uwsCompat(),
  serverType: "uWebSockets",
});

app.set("etag", false);
app.set("x-powered-by", false);

app.get("/", function (req, res) {
  res.sendLite("");
});

app.get("/user/:id", function (req, res) {
  res.sendLite(req.params.id);
});

app.post("/user", function (req, res) {
  res.sendLite("");
});

app.listen(3000, function () {});
