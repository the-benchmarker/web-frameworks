const restana = require("restana");
const service = restana({
  disableResponseEvent: true
});

service.get("/", function(req, res) {
  res.send(200);
});

service.get("/user/:id", function(req, res) {
  res.send(req.params.id);
});

service.post("/user", function(req, res) {
  res.send(200);
});

service.start(3000);
