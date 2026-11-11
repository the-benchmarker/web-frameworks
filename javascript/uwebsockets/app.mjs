import { hash } from 'node:crypto';
import { createRequire } from 'node:module';

// Use the CommonJS entry point to bypass the broken ESM wrapper in 20.71.0
// (fixed by https://github.com/uNetworking/uWebSockets.js/pull/1320, not yet in a tagged release).
const require = createRequire(import.meta.url);
const { App, DeclarativeResponse, _cfg } = require('uWebSockets.js');

// Drop the "uWebSockets: 20" response header
_cfg('silent');

// Create a new instance of uWebSockets.js HTTP
const port = 3000;
const app = App();

const ID = /^[0-9]{8}$/;
const SEED = /^[a-z]{8}$/;
const SEED_QUERY = /^seed=([a-z]{8})$/;
const BOUNDARY = /;\s*boundary\s*=\s*(?:"([^"]*)"|([^\s;]*))/i;
const DISPOSITION = /^content-disposition\s*:\s*form-data\s*(;.*)?$/im;
const PARAM = /;\s*([^\s=;]+)\s*=\s*(?:"((?:[^"\\]|\\.)*)"|([^\s;]*))/g;
const HEADER_END = Buffer.from('\r\n\r\n');

// Upper bound for buffered request bodies; the upload load payload is 256 KiB plus framing
const MAX_BODY = 1024 * 1024;

const BAD_REQUEST = '400 Bad Request';
const PAYLOAD_TOO_LARGE = '413 Payload Too Large';
const UNSUPPORTED_MEDIA_TYPE = '415 Unsupported Media Type';

function decode(value) {
  try {
    return decodeURIComponent(value);
  } catch {
    return null;
  }
}

// Lowercased media type without parameters
function mediaType(contentType) {
  const i = contentType.indexOf(';');
  return (i === -1 ? contentType : contentType.slice(0, i)).trim().toLowerCase();
}

// application/json, any +json, text/x-json and application/jsonrequest
function isJson(contentType) {
  const type = mediaType(contentType);
  return type === 'application/json' || type.endsWith('+json') || type === 'text/x-json' || type === 'application/jsonrequest';
}

// Buffer the whole request body (Content-Length or chunked) with uWS collectBody.
// The handler must return a status line, or a value to send as JSON.
// collectBody never calls back for an aborted request, but uWS still requires onAborted.
function withBody(res, handler) {
  res.onAborted(() => {});
  res.collectBody(MAX_BODY, (body) => {
    const result = body === null ? PAYLOAD_TOO_LARGE : handler(Buffer.from(body));
    if (typeof result === 'string') res.writeStatus(result).end();
    else res.writeHeader('Content-Type', 'application/json').end(JSON.stringify(result));
  });
}

// Returns { count, checksum } or null when the payload breaks the schema
function summarizeItems(payload) {
  const items = payload?.items;
  if (!Array.isArray(items) || items.length !== 100) return null;
  let checksum = 0;
  for (let i = 0; i < 100; i++) {
    const id = items[i]?.id;
    if (!Number.isInteger(id) || id < 0 || id > 1000000) return null;
    checksum += id;
  }
  return { count: 100, checksum };
}

// Parameters of a Content-Disposition header, keys lowercased
function dispositionParams(headers) {
  const match = DISPOSITION.exec(headers);
  if (match === null) return null;
  const params = {};
  for (const [, key, quoted, token] of (match[1] ?? '').matchAll(PARAM)) {
    params[key.toLowerCase()] = quoted === undefined ? token : quoted.replace(/\\(.)/g, '$1');
  }
  return params;
}

// Minimal RFC 7578 multipart/form-data parser. uWS.getParts() is not used because it
// matches the boundary without the leading CRLF (truncating file data that contains
// "--boundary"), rejects quoted boundaries and silently drops incomplete trailing parts.
// Returns the single uploaded "file" part as a Buffer view, or null when the body is
// malformed or does not contain exactly one uploaded file.
function uploadedFile(body, boundary) {
  const delimiter = Buffer.from(`\r\n--${boundary}`, 'latin1');
  let pos;
  // The first delimiter may start the body without the preceding CRLF
  if (body.indexOf(delimiter.subarray(2)) === 0) {
    pos = delimiter.length - 2;
  } else {
    const first = body.indexOf(delimiter);
    if (first === -1) return null;
    pos = first + delimiter.length;
  }

  let file = null;
  for (;;) {
    // "--" marks the close delimiter; any epilogue after it is ignored
    if (body[pos] === 0x2d && body[pos + 1] === 0x2d) return file;
    if (body[pos] !== 0x0d || body[pos + 1] !== 0x0a) return null;

    const headersEnd = body.indexOf(HEADER_END, pos);
    if (headersEnd === -1) return null;
    const dataStart = headersEnd + HEADER_END.length;
    const dataEnd = body.indexOf(delimiter, dataStart);
    if (dataEnd === -1) return null;

    const params = dispositionParams(body.toString('latin1', pos + 2, headersEnd));
    if (params !== null && params.name === 'file') {
      // A part without filename is a text field: it cannot be the uploaded file.
      // filename="" is an unselected file input and is ignored.
      if (params.filename === undefined) return null;
      if (params.filename !== '') {
        if (file !== null) return null;
        file = body.subarray(dataStart, dataEnd);
      }
    }
    pos = dataEnd + delimiter.length;
  }
}

// GET "/" => 200 with empty body
app.get('/', new DeclarativeResponse().end());

// GET "/user/:id" => 200 with {"id": id}, the id must be eight ASCII digits
app.get('/user/:id', (res, req) => {
  let id = req.getParameter(0);
  if (!ID.test(id)) {
    id = decode(id);
    if (id === null || !ID.test(id)) {
      res.writeStatus(BAD_REQUEST).end();
      return;
    }
  }
  res.writeHeader('Content-Type', 'application/json').end(JSON.stringify({ id }));
});

// POST "/user" => 200 with empty body
app.post('/user', new DeclarativeResponse().end());

// GET "/serialization?seed=xxxxxxxx" => 200 with 100 {"seed": seed} objects
app.get('/serialization', (res, req) => {
  const query = req.getQuery() ?? '';
  let seed = SEED_QUERY.exec(query)?.[1];
  if (seed === undefined) {
    // Slow path for percent-encoded names or values; any other parameter is rejected
    const eq = query.indexOf('=');
    const valid = eq !== -1 && !query.includes('&') && decode(query.slice(0, eq)) === 'seed';
    seed = valid ? decode(query.slice(eq + 1)) : null;
    if (seed === null || !SEED.test(seed)) {
      res.writeStatus(BAD_REQUEST).end();
      return;
    }
  }
  const items = new Array(100);
  for (let i = 0; i < 100; i++) items[i] = { seed };
  res.writeHeader('Content-Type', 'application/json').end(JSON.stringify(items));
});

// POST "/deserialization" => 200 with {"count": 100, "checksum": sum of ids}
app.post('/deserialization', (res, req) => {
  // Headers and query must be read now: req is only valid inside this callback
  const error = !isJson(req.getHeader('content-type')) ? UNSUPPORTED_MEDIA_TYPE
    : req.getQuery() ? BAD_REQUEST : null;
  withBody(res, (body) => {
    if (error) return error;
    try {
      return summarizeItems(JSON.parse(body.toString('utf8'))) ?? BAD_REQUEST;
    } catch {
      return BAD_REQUEST;
    }
  });
});

// POST "/upload" => 200 with first/last byte and SHA-256 of the uploaded "file" part
app.post('/upload', (res, req) => {
  const contentType = req.getHeader('content-type');
  const boundary = BOUNDARY.exec(contentType)?.slice(1).find((value) => value !== undefined) ?? '';
  const error = mediaType(contentType) !== 'multipart/form-data' ? UNSUPPORTED_MEDIA_TYPE
    : boundary === '' || boundary.length > 70 ? BAD_REQUEST : null;
  withBody(res, (body) => {
    if (error) return error;
    const file = uploadedFile(body, boundary);
    if (file === null) return BAD_REQUEST;
    const last = file.length - 1;
    return {
      first_byte: last === -1 ? null : file[0],
      last_byte: last === -1 ? null : file[last],
      checksum: hash('sha256', file),
    };
  });
});

// Start the server on port 3000
app.listen(port, (token) => {
  if (!token) {
    console.error(`Failed to listen on port ${port}`);
    process.exit(1);
  }
});
