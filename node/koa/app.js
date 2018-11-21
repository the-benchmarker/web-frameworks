const Koa = require('koa');
const Router = require('koa-router');

let app = new Koa();
let router = new Router();

router
  .get('/', (ctx, next) => {
    ctx.body = '';
  })
  .get('/user/:id', (ctx, next) => {
    ctx.body = ctx.params.id;
  })
  .post('/user', (ctx, next) => {
    ctx.body = '';
  });

app
  .use(router.routes())
  .use(router.allowedMethods())
  .listen(80);
