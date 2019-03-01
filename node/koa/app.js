var Koa = require("koa");
var Router = require("koa-router");

var app = new Koa();
var router = new Router();

router
  .get("/", (ctx, next) => {
    ctx.body = "";
  })
  .get("/user/:id", (ctx, next) => {
    ctx.body = ctx.params.id;
  })
  .post("/user", (ctx, next) => {
    ctx.body = "";
  });

app
  .use(router.routes())
  .use(router.allowedMethods())
  .listen(3000);
