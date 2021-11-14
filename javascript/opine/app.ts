import { opine } from "https://deno.land/x/opine@1.9.1/mod.ts";

const app = opine();

app.get("/", function (_, res) {
  res.send("");
});

app.get("/user/:id", (req, res) => {
  res.end(req.params.id);
});

app.post("/user", (_, res) => {
  res.end("");
});

app.listen("0.0.0.0:3000");
