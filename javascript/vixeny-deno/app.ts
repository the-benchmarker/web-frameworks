import { wrap } from "jsr:@vixeny/core@0.1.42";

export default wrap()()
  .get({
    path: "/",
    f: () => null,
  })
  .get({
    path: "/id/:id",
    param: {
      unique: true,
    },
    f: (f) => f.param,
  })
  .post({
    path: "/user",
    f: () => null,
  });

Deno.serve({ fetch: await root.compose(), port: 3000 }, app);
