module.exports.routes = {
  'GET /': { action: 'home' },
  'GET /user/:id': { action: 'user/get' },
  'POST /user/': { action: 'user/post' },
};
