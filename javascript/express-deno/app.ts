import express from 'npm:express@5';

const app = express();

app.disable('etag');
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

const server = app.listen(3000);
server.keepAliveTimeout = 0;
