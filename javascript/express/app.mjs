import express from 'express';

const app =express()
app.set('etag', false);

app.get('/', function (req, res) {
  res.send('');
});

app.get('/user/:id', function (req, res) {
  res.send(req.params.id);
});

app.post('/user', function (req, res) {
  res.send('');
});

app.listen(3000, function (err, address) {
  if (err) {
    app.log.error(err);
    process.exit(1);
  }
  console.info(`Worker PID ${process.pid} is listening at http://localhost:3000`);
});
