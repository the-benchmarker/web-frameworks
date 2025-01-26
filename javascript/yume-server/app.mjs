import { Yume } from 'yume-server';

const yume = new Yume();

yume.get('/', function (req, res) {
  res.end('');
});

yume.get('/user/:id', function (req, res) {
  res.end(req.getParams().id);
});

yume.post('/user', function (_req, res) {
  res.end('');
})
yume.listen('0.0.0.0', 3000)
