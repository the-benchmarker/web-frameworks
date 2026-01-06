import express from 'express';

const app = express();
app.set('etag', false);
app.set('x-powered-by', false);

app.get('/', (req, res) => {
  res.end('');
});

app.get('/user/:id', (req, res) => {
  res.end(req.params.id);
});

app.post('/user', (req, res) => {
  res.end('');
});

const server = app.listen(3000);
server.keepAliveTimeout = 0;
