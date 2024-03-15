const { BlazeCreator } = require('@busy-hour/blaze');

const list = BlazeCreator.action({
  rest: 'GET /',
  handler(ctx) {
    ctx.response = 'text';

    return '';
  },
});

const service = BlazeCreator.service({
  name: '',
  actions: {
    list,
  },
});

module.exports = service;
