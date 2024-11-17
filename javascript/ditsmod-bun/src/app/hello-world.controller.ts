import { controller, SingletonRequestContext, route } from '@ditsmod/core';

@controller({ isSingleton: true })
export class HelloWorldController {
  @route('GET')
  empty(ctx: SingletonRequestContext) {
    ctx.nodeRes.end();
  }

  @route('GET', 'user/:id')
  userId(ctx: SingletonRequestContext) {
    ctx.nodeRes.end(ctx.pathParams!.id);
  }

  @route('POST', 'user')
  postHello(ctx: SingletonRequestContext) {
    ctx.nodeRes.end();
  }
}
