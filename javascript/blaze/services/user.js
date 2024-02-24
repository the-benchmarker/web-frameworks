const { BlazeCreator } = require('@busy-hour/blaze');

const find = BlazeCreator.action({
  rest: 'GET /:id',
  handler(ctx) {
    ctx.response.set('text');

    return ctx.request.params?.id;
  },
});

const create = BlazeCreator.action({
  rest: 'POST /',
  handler(ctx) {
    ctx.response.set('text');

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

module.exports = service;
