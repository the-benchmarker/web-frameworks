const restify = require('restify');

const server = restify.createServer();

server.use(restify.plugins.acceptParser(server.acceptable));
server.use(restify.plugins.queryParser());
server.use(restify.plugins.bodyParser());

server.get('/', function (req, res, next) {
  res.end();
  return next();
});

server.get('/user/:id', function (req, res, next) {
  res.end(req.params.id);
  return next();
});

server.post('/user', function (req, res, next) {
  res.end();
  return next();
});

server.listen(3000, function () {
  console.log('%s listening at %s', server.name, server.url);
});
