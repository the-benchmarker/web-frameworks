import Koa from "koa";
import Router from "koa-router";

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

export {app,router};
