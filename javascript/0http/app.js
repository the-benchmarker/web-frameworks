const cero = require("0http");
const { router, server } = cero();

router.on("GET", "/", (req, res) => {
  res.end();
});

router.on("GET", "/user/:id", (req, res, params) => {
  res.end(params.id);
});

router.on("POST", "/user", (req, res) => {
  res.end();
});

server.listen(3000);
