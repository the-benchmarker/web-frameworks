const nanoexpress = require("nanoexpress");

nanoexpress()
  .get("/", (req, res) => {
    res.end("");
  })
  .get("/user/:id", (req, res) => {
    res.end(req.params.id);
  })
  .post("/user", (req, res) => {
    res.end("");
  })
  .listen(3000);
