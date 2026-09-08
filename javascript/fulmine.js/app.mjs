import express from 'fulmine.js';

// One worker per usable core, forked by the framework: each of them binds port 3000 with
// uWebSockets.js's shared flag, which is SO_REUSEPORT, so the kernel hands each connection to one
// of them and the primary process is not in the path. "auto" reads the cgroup quota before the
// machine, so a container gets its own cores and not the host's.
const app = express({ cluster: 'auto' });

app.set('etag', false);
// keep-alive is implicit on HTTP/1.1, so the Connection and Keep-Alive headers carry no
// information: off, which is also what the engine itself answers.
app.set('connection headers', false);

app.get('/', function (req, res) {
  res.send('');
});

app.get('/user/:id', function (req, res) {
  res.send(req.params.id);
});

app.post('/user', function (req, res) {
  res.send('');
});

// this file runs again in every worker, and the primary only forks: the listen below happens once
// per worker
app.listen(3000);
