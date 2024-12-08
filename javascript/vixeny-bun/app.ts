import { wrap } from "vixeny";

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
    f: () => ""
  })
  .compose()
  
export default { 
    port: 3000,
    fetch: app,
    reusePort: true
} 
