import { hash } from 'node:crypto';
import express from 'fulmine.js';

// The same routes as the fulmine.js entry, written with app.uwsLite: an (req, res) handler that
// uWebSockets.js serves over two small objects. https://fulmine.sndesign.it/uws-routes#lite-routes-app-uwslite
// One worker per usable core, forked by the framework, all on port 3000 with SO_REUSEPORT.
const app = express({ cluster: 'auto' });

app.set('etag', false);
app.set('connection headers', false);

const ID = /^[0-9]{8}$/;
const SEED = /^[a-z]{8}$/;

/** application/json, any +json, text/x-json and application/jsonrequest, as the express entry. */
function isJson(type) {
  if (type === undefined) return false;
  const semi = type.indexOf(';');
  const mime = (semi === -1 ? type : type.slice(0, semi)).trim().toLowerCase();
  return mime === 'application/json' || mime.endsWith('+json') || mime === 'text/x-json' || mime === 'application/jsonrequest';
}

// the same empty 200 every time: written once at startup, and µWS answers without entering JavaScript
app.uws.get('/', new express.uWS.DeclarativeResponse().end(''));
app.uws.post('/user', new express.uWS.DeclarativeResponse().end(''));

app.uwsLite.get('/user/:id', function (req, res) {
  const { id } = req.params;
  if (!ID.test(id)) return res.sendStatus(400);
  res.json({ id });
});

app.uwsLite.get('/serialization', function (req, res) {
  const { query } = req;
  const { seed } = query;
  if (typeof seed !== 'string' || !SEED.test(seed) || Object.keys(query).length !== 1) return res.sendStatus(400);
  const rows = new Array(100);
  for (let i = 0; i < 100; i++) rows[i] = { seed };
  res.json(rows);
});

// the route accepts any type, so the type is checked here, before the body is parsed
app.uwsLite.post('/deserialization', { body: 'text', limit: '1mb', headers: ['content-type'] }, function (req, res) {
  if (!isJson(req.get('content-type'))) return res.sendStatus(415);
  if (Object.keys(req.query).length !== 0) return res.sendStatus(400);
  let body;
  try {
    body = JSON.parse(req.body);
  } catch {
    return res.sendStatus(400);
  }
  const items = body?.items;
  if (!Array.isArray(items) || items.length !== 100) return res.sendStatus(400);
  let checksum = 0;
  for (let i = 0; i < 100; i++) {
    const id = items[i]?.id;
    if (!Number.isInteger(id) || id < 0 || id > 1000000) return res.sendStatus(400);
    checksum += id;
  }
  res.json({ count: 100, checksum });
});

// a body that is not multipart/form-data is not read and leaves req.body undefined, a malformed one is a 400
app.uwsLite.post('/upload', { body: 'multipart', limit: '1mb' }, function (req, res) {
  if (req.body === undefined) return res.sendStatus(415);
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

app.listen(3000);
