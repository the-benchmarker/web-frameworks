import { hash } from 'node:crypto';
import { availableParallelism } from 'node:os';
import { createApp } from '@morojs/moro';

process.env.LOG_LEVEL = 'error';
process.env.NODE_ENV = 'production';

const app = await createApp({
  server: {
    port: 3000,
    host: '0.0.0.0',
    engine: 'moro', // Moro's native engine (@morojs/engine)
    requestTracking: { enabled: false }, // no other entry tracks requests
    requestLogging: { enabled: false }, // no other entry logs requests
    errorBoundary: { enabled: false },
  },
  performance: {
    clustering: {
      enabled: true,
      // One worker per core the container may actually run on. Moro's 'auto'
      // counts os.cpus(), which is the whole host even under --cpuset-cpus;
      // availableParallelism() honours the affinity mask, the same source the
      // other Node entries' cluster.mjs use.
      workers: availableParallelism(),
    },
  },
  logger: { level: 'warn' },
});

// SCENARIO.md: every ID and seed is exactly eight characters. JS's `$` also
// matches before a trailing newline, so the length is checked explicitly.
const ID = /^[0-9]{8}$/;
const SEED = /^[a-z]{8}$/;
const isId = s => typeof s === 'string' && s.length === 8 && ID.test(s);
const isSeed = s => typeof s === 'string' && s.length === 8 && SEED.test(s);

const mediaType = contentType => {
  if (!contentType) return '';
  const semi = contentType.indexOf(';');
  return (semi === -1 ? contentType : contentType.slice(0, semi)).trim().toLowerCase();
};
const JSON_TYPES = new Set(['application/json', 'text/x-json', 'application/jsonrequest']);
const isJsonType = type => JSON_TYPES.has(type) || (type.startsWith('application/') && type.endsWith('+json'));

// A literal body in place of a handler is answered inside @morojs/engine
// without entering JS, the same way elysia-bun's literal handlers are served
// by Bun's static routes.
app.get('/').handler('');
app.post('/user').handler('');

// The route parameter arrives percent-decoded (/user/%30%30%30%30%30%30%34%32
// is 00000042); anything but eight ASCII digits is a 400.
app.get('/user/:id').handler((req, res) => {
  const { id } = req.params;
  if (!isId(id)) return res.status(400).end();
  res.json({ id });
});

// Exactly one query parameter, named seed after decoding, eight lowercase
// letters. req.query is a decoded last-wins map, so the raw query string is
// what shows a repeated name; '+' decodes to a space and fails the letter rule.
app.get('/serialization').handler((req, res) => {
  const url = req.url;
  const q = url.indexOf('?');
  if (q === -1 || url.indexOf('&', q) !== -1) return res.status(400).end();
  const seed = req.query.seed;
  if (!isSeed(seed) || Object.keys(req.query).length !== 1) return res.status(400).end();
  const rows = new Array(100);
  for (let i = 0; i < 100; i++) rows[i] = { seed };
  res.json(rows);
});

// application/json bodies arrive parsed; the other JSON media types arrive
// as the framework's text (+json suffixes, text/x-json) or bytes (the
// legacy application/jsonrequest) and are parsed here. A body that is not
// JSON by its media type is a 415 before anything is looked at; any query
// parameter is a 400.
app.post('/deserialization').handler((req, res) => {
  const type = mediaType(req.headers['content-type']);
  if (!isJsonType(type)) return res.status(415).end();
  if (req.url.indexOf('?') !== -1) return res.status(400).end();
  let doc = req.body;
  if (typeof doc === 'string' || Buffer.isBuffer(doc)) {
    try {
      doc = JSON.parse(doc.toString());
    } catch {
      return res.status(400).end();
    }
  }
  if (doc === null || doc === undefined || typeof doc !== 'object' || Array.isArray(doc)) {
    return res.status(400).end();
  }
  const items = doc.items;
  if (!Array.isArray(items) || items.length !== 100) return res.status(400).end();
  let checksum = 0;
  for (let i = 0; i < 100; i++) {
    const item = items[i];
    if (item === null || typeof item !== 'object' || Array.isArray(item)) return res.status(400).end();
    const id = item.id;
    // Number.isInteger accepts 2.0 and 2e0 (the integer 2) and rejects 1.5,
    // strings, booleans, null, arrays and objects.
    if (!Number.isInteger(id) || id < 0 || id > 1000000) return res.status(400).end();
    checksum += id;
  }
  res.json({ count: 100, checksum });
});

// The framework parses multipart/form-data into req.body = { fields, files,
// parts }; parts is the ordered list of every part, which is what tells a
// repeated `file` part from a single one. A part named file with no filename
// is a text field; one with filename="" is an unselected file input.
app.post('/upload').handler((req, res) => {
  if (mediaType(req.headers['content-type']) !== 'multipart/form-data') return res.status(415).end();
  const body = req.body;
  if (!body || typeof body !== 'object' || !Array.isArray(body.parts)) return res.status(400).end();
  let file = null;
  for (const part of body.parts) {
    if (part.name !== 'file') continue;
    if (part.filename === undefined) return res.status(400).end();
    if (part.filename === '') continue;
    if (file !== null) return res.status(400).end();
    file = part.data;
  }
  if (file === null) return res.status(400).end();
  res.json({
    first_byte: file.length === 0 ? null : file[0],
    last_byte: file.length === 0 ? null : file[file.length - 1],
    checksum: hash('sha256', file),
  });
});

// The framework parses request bodies by Content-Type before routing and
// answers a malformed one with 400. SCENARIO.md wants an unsupported media
// type answered 415 before the body is looked at, so for /upload a body that
// failed to parse as something other than multipart is reported as the media
// type problem it is; everything else keeps the framework's answer.
app.setErrorHandler((err, req, res) => {
  if (err?.statusCode === 400 && req.method === 'POST' && req.url.startsWith('/upload')) {
    const type = mediaType(req.headers['content-type']);
    return res.status(type === 'multipart/form-data' ? 400 : 415).end();
  }
  if (err?.statusCode === 400 || err?.statusCode === 413) return res.status(err.statusCode).end();
  res.status(500).end();
});

app.listen();
