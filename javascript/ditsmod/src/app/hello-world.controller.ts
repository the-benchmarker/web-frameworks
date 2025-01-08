import { controller, RequestContext } from '@ditsmod/core';
import { route } from '@ditsmod/routing';

@controller({ scope: 'ctx' })
export class HelloWorldController {
  @route('GET')
  empty(ctx: RequestContext) {
    ctx.rawRes.end();
  }

  @route('GET', 'user/:id')
  userId(ctx: RequestContext) {
    ctx.rawRes.end(ctx.pathParams!.id);
  }

  @route('POST', 'user')
  postHello(ctx: RequestContext) {
    ctx.rawRes.end();
  }
}
