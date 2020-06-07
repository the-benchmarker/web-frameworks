var Koa = require("koa");
var Router = require("koa-router");

var app = new Koa();
var router = new Router();

router
  .get("/", (ctx) => {
    ctx.body = "";
  })
  .get("/user/:id", (ctx) => {
    ctx.body = ctx.params.id;
  })
  .post("/user", (ctx) => {
    ctx.body = "";
  });

app.use(router.routes()).use(router.allowedMethods()).listen(3000);
