var { App } = require('@ionited/mesh');
var app = new App();

app

  .get('/', function (_, res) {
    res.end();
  })

  .get('/user/:id', function (req, res) {
    res.send(req.params().id);
  })

  .post('/user', function (_, res) {
    res.end();
  })

  .listen(3000);
