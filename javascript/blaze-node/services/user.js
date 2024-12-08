import { BlazeCreator } from '@busy-hour/blaze';

const find = BlazeCreator.action({
  rest: 'GET /:id',
  async handler(ctx) {
    ctx.response = 'text';

    return ctx.request.params?.id;
  },
});

const create = BlazeCreator.action({
  rest: 'POST /',
  async handler(ctx) {
    ctx.response = 'text';

    return '';
  },
});

const service = BlazeCreator.service({
  name: 'user',
  actions: {
    find,
    create,
  },
});

export default service;
