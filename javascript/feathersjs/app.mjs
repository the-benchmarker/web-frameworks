import feathers from '@feathersjs/feathers';
import express from '@feathersjs/express';

const app = express(feathers());

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

app.listen(3000, function () {});
