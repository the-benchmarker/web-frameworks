import { Elysia } from "elysia";

new Elysia()
  .get("/", () => "")
  .get("/user/:id", (req) => req.params.id)
  .post("/user", () => "")
  .listen(3000);
