import { App } from "@tinyhttp/app";

const app = new App();

app
  .get("/", (_, res) => {
    res.send("");
  })
  .get("/user/:id", (req, res) => {
    res.send(`${req.params.id}`);
  })
  .post("/user", (req, res) => {
    res.end("");
  })
  .listen(3000);
