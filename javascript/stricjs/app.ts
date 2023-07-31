import { Router, macro } from "@stricjs/router";

export default new Router()
  .get(
    "/",
    macro(() => new Response())
  )
  .get("/user/:id", (req) => new Response(req.params.id))
  .post(
    "/user",
    macro(() => new Response())
  );
