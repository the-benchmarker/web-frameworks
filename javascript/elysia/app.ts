import { Elysia } from "elysia";

const app = new Elysia();

app.get("/", () => "")

app.get("/user/:id", req => req.params.id);

app.post("/user", () => "");

app.listen(3000);