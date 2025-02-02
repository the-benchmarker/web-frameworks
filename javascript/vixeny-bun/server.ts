import { wrap } from "vixeny";

export default wrap()()
  .get({
    path: "/",
    f: () => null,
  })
  .get({
    path: "/user/:id",
    param: {
      unique: true,
    },
    f: (f) => f.param,
  })
  .post({
    path: "/user",
    f: () => null,
  });
