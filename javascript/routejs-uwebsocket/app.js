import { Router } from "@routejs/router";
import uWS from "uWebSockets.js";

const app = new Router();

app.get("/", function (req, res) {
  res.end("");
});
app
  .get("/", (req, res) => res.end(""))
  .get("/user/{id}", (req, res) => res.end(req.params.id))
  .post("/user", (req, res) => res.end(""))
  .use((req, res) => res.writeStatus("404").end(""));

uWS
  .App()
  .any("/*", (res, req) => {
    const handler = app.handler();
    handler(req, res);
  })
  .listen(3000, () => {
    console.log("Server started on port 3000");
  });
