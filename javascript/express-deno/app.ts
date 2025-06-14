import express from "express";
import { createFetchHandler } from "./fetcher.ts"; // path to the code above
const app = express();
app.set("etag", false);

app.get("/", function (req, res) {
  res.send("");
});

app.get("/user/:id", function (req, res) {
  res.send(req.params.id);
});

app.post("/user", function (req, res) {
  res.send("");
});

const fetchHandler = createFetchHandler(app);

export default { fetch: fetchHandler };
