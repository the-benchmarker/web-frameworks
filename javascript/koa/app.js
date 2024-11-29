var Koa = require('koa');
var Router = require('koa-router');
var cluster = require('cluster');
var os = require('os');

var app = new Koa();
var router = new Router();

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

if (cluster.isPrimary) {
  var numCPUs = os.cpus().length;
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
    console.log(`Worker ${worker.process.pid} died`);
    cluster.fork();
  });
} else {
  app
    .use(router.routes())
    .use(router.allowedMethods())
    .listen(3000, () => {
      console.log(`Worker ${process.pid} started`);
    });
}
