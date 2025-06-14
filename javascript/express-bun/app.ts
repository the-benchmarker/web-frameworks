import express from "express";

const app = express();
app.set("etag", false);

app.get("/", (req, res) => {
  res.send("");
});

app.get("/user/:id", (req, res) => {
  res.send(req.params.id);
});

app.post("/user", (req, res) => {
  res.send("");
});

app.listen(3000);
