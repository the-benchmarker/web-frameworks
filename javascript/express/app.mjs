import express from 'express';

const app =express()
app.set('etag', false);
app.set('x-powered-by', false);

app.get('/', function (req, res) {
  res.end('');
});

app.get('/user/:id', function (req, res) {
  res.end(req.params.id);
});

app.post('/user', function (req, res) {
  res.end('');
});

const server = app.listen(3000, function (err, address) {
  if (err) {
    app.log.error(err);
    process.exit(1);
  }
  console.info(`Worker PID ${process.pid} is listening at http://localhost:3000`);
});
server.keepAliveTimeout = 0;
