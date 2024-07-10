import { wrap } from "jsr:@vixeny/core@0.1.42";

const app  = wrap()()
  .stdPetition({
    path: '/',
    f: () => null
  })
  .stdPetition({
    path: '/user/:id',
    param: {
      unique: true
    },
    f: ctx => ctx.param
  })
  .stdPetition({
    path: '/user',
    f: () => null
  })
  .compose()

<<<<<<< HEAD
Deno.serve({port: 3000}, app)
=======
  Deno.serve({port: 3000}, app)
>>>>>>> 8334be01 (`app` doesn't have `fetch`)
