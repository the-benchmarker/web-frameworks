import { controller, SingletonRequestContext } from '@ditsmod/core';
import { route } from '@ditsmod/routing';

@controller({ scope: 'module' })
export class HelloWorldController {
  @route('GET')
  empty(ctx: SingletonRequestContext) {
    ctx.rawRes.end();
  }

  @route('GET', 'user/:id')
  userId(ctx: SingletonRequestContext) {
    ctx.rawRes.end(ctx.pathParams!.id);
  }

  @route('POST', 'user')
  postHello(ctx: SingletonRequestContext) {
    ctx.rawRes.end();
  }
}
