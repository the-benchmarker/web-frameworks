const { App } = require("@sifrr/server");

const app = new App();

app
  .get("/", (res) => {
    res.end("");
  })
  .get("/user/:id", (res, req) => {
    res.end(req.getParameter(0));
  })
  .post("/user", (res) => {
    res.end("");
  })
  .listen(3000);

console.info(`Worker PID ${process.pid} is listening at http://localhost:3000`);
