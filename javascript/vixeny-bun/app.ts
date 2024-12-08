import { wrap } from "vixeny";

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
    method: 'POST',
    path: '/user',
    f: () => null
  })
  .compose()

export default { 
    port: 3000,
    fetch: app,
    reusePort: true
} 
