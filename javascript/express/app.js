import express  from 'express';

const app = express();
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

export default app;
