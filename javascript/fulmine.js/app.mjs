import { hash } from 'node:crypto';
import express from 'fulmine.js';

// One worker per usable core, forked by the framework: each of them binds port 3000 with
// uWebSockets.js's shared flag, which is SO_REUSEPORT, so the kernel hands each connection to one
// of them and the primary process is not in the path. "auto" reads the cgroup quota before the
// machine, so a container gets its own cores and not the host's.
const app = express({ cluster: 'auto' });

app.set('etag', false);
// no load request is conditional, and the spec wants If-None-Match: * to do the work and answer 200
app.set('conditional get', false);
// keep-alive is implicit on HTTP/1.1, so the Connection and Keep-Alive headers carry no
// information: off, which is also what the engine itself answers.
app.set('connection headers', false);

const ID = /^[0-9]{8}$/;
const SEED = /^[a-z]{8}$/;
const JSON_TYPES = ['application/json', '+json', 'text/x-json', 'application/jsonrequest'];

// end() is node's, so no Content-Type goes out: send('') would write text/html on an empty body,
// as Express does, and that is 40 bytes per response the benchmark never reads
app.get('/', function (req, res) {
  res.end('');
});

app.get('/user/:id', function (req, res) {
  const { id } = req.params;
  if (!ID.test(id)) return res.sendStatus(400);
  res.json({ id });
});

app.post('/user', function (req, res) {
  res.end('');
});

app.get('/serialization', function (req, res) {
  const { query } = req;
  const { seed } = query;
  if (typeof seed !== 'string' || !SEED.test(seed) || Object.keys(query).length !== 1) return res.sendStatus(400);
  const rows = new Array(100);
  for (let i = 0; i < 100; i++) rows[i] = { seed };
  res.json(rows);
});

app.post('/deserialization', express.json({ type: JSON_TYPES }), function (req, res) {
  // req.is is false on the wrong type and null on an empty body, and the parser skipped both
  if (req.body === undefined) return res.sendStatus(req.is(JSON_TYPES) === false ? 415 : 400);
  const { items } = req.body;
  if (Object.keys(req.query).length !== 0 || !Array.isArray(items) || items.length !== 100) return res.sendStatus(400);
  let checksum = 0;
  for (let i = 0; i < 100; i++) {
    const id = items[i]?.id;
    if (!Number.isInteger(id) || id < 0 || id > 1000000) return res.sendStatus(400);
    checksum += id;
  }
  res.json({ count: 100, checksum });
});

app.post('/upload', express.multipart({ limit: '1mb' }), function (req, res) {
  if (req.body === undefined) return res.sendStatus(req.is('multipart/form-data') === false ? 415 : 400);
  const uploads = req.files.filter((file) => file.fieldname === 'file');
  // a part named file with no filename is a text field, and lands in req.body
  if (uploads.length !== 1 || req.body.file !== undefined) return res.sendStatus(400);
  const { buffer, size } = uploads[0];
  res.json({
    first_byte: size === 0 ? null : buffer[0],
    last_byte: size === 0 ? null : buffer[size - 1],
    checksum: hash('sha256', buffer),
  });
});

// the default handler logs, and the scenario wants the logging off
app.use(function (err, req, res, next) {
  res.sendStatus(err.status ?? 500);
});

// this file runs again in every worker, and the primary only forks: the listen below happens once
// per worker
app.listen(3000);
