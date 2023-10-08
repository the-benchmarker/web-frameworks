const { App } = require('@sifrr/server');

const app = new App();

app
  .get('/', (res) => {
    res.end('');
  })
  .get('/user/:id', (res, req) => {
    res.end(req.getParameter(0));
  })
  .post('/user', (res) => {
    res.end('');
  })
  .listen(3000);
