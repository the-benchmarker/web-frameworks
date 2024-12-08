import { wrap } from "jsr:@vixeny/core@0.1.53";

const app  = await wrap()()
  .get({
    path: '/',
    f: () => ''
  })
  .get({
    path: '/user/:id',
    param: {
      unique: true
    },
    f: ctx => ctx.param
  })
  .post({
    path: '/user',
    f: () => ''
  })
  .compose()

Deno.serve({port: 3000}, app)