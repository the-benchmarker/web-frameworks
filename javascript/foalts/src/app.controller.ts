import { Context, Get, HttpResponseOK, Post } from "@foal/core";

export class AppController {
  @Get("/")
  index() {
    return new HttpResponseOK("");
  }

  @Get("/user/:id")
  getUser(ctx: Context) {
    return new HttpResponseOK(ctx.request.params.id);
  }

  @Post("/user")
  postUser() {
    return new HttpResponseOK("");
  }
}
