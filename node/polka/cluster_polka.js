const { cpus } = require('os');
const cluster = require('cluster');
const polka = require('polka');

let i=0, len=cpus().length;

if (cluster.isMaster) {
  for (; i < len; i++) {
    cluster.fork();
  }
} else {
  polka()
    .get('/', (req, res) => {
      res.end('');
    })
    .get('/user/:id', (req, res) => {
      res.end(req.params.id);
    })
    .post('/user', (req, res) => {
      res.end('');
    })
    .listen(3000);
}
