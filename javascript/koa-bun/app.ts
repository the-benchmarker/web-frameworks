import Koa from 'koa';
import Router from 'koa-router';

const app = new Koa();
const router = new Router();

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

const server = app
    .use(router.routes())
    .listen(3000);

server.keepAliveTimeout = 0