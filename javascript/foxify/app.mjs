import Foxify from 'foxify';

const app = new Foxify();

app.disable('x-powered-by').set('url', '0.0.0.0').set('port', 3000);

app
  .get('/', (req, res) => res.send(''))
  .get('/user/:id', (req, res) => res.send(req.params.id))
  .post('/user', (req, res) => res.send(''));

app.start();
