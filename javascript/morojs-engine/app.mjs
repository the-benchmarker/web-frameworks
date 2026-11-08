import { hash } from 'node:crypto';
import engine from '@morojs/engine';

// Raw @morojs/engine, no framework on top: the same role javascript/uwebsockets
// plays for uWebSockets.js. The engine parses HTTP natively (request line,
// headers, Content-Length and chunked bodies) and hands each request to
// onRequest as (reqId, methodIdx, path); everything above that - route
// matching, query and body validation, multipart framing - is this file's.

// Method indices in the engine's table: GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS, OTHER
const GET = 0;
const POST = 1;
const USER_PREFIX = '/user/';

const JSON_HEADERS = ['content-type', 'application/json'];
const ID = /^[0-9]{8}$/;
const SEED = /^[a-z]{8}$/;

// SCENARIO.md: every ID and seed is exactly eight characters. JS's `$` also
// matches before a trailing newline, so the length is checked explicitly.
const isId = s => s.length === 8 && ID.test(s);
const isSeed = s => s.length === 8 && SEED.test(s);

const decode = s => {
  try {
    return decodeURIComponent(s);
  } catch {
    return null; // a malformed escape is not a valid value of anything
  }
};

const status = (reqId, code) => engine.respond(reqId, code, null, null);
const json = (reqId, body) => engine.respond(reqId, 200, JSON_HEADERS, body);

// ---- GET /user/{id} ----------------------------------------------------------
function user(reqId, rawId) {
  const id = decode(rawId);
  if (id === null || !isId(id)) return status(reqId, 400);
  json(reqId, `{"id":"${id}"}`);
}

// ---- GET /serialization?seed= ----------------------------------------------
// Exactly one query parameter, named seed after decoding, eight lowercase
// letters. The raw query string is read so a repeated name is seen as two
// parameters rather than collapsed; '+' decodes to a space (application/x-www-
// form-urlencoded) and therefore fails the letter rule.
function serialization(reqId, query) {
  if (!query || query.indexOf('&') !== -1) return status(reqId, 400);
  const eq = query.indexOf('=');
  if (eq === -1) return status(reqId, 400);
  const name = decode(query.slice(0, eq));
  const value = decode(query.slice(eq + 1).replace(/\+/g, ' '));
  if (name !== 'seed' || value === null || !isSeed(value)) return status(reqId, 400);
  json(reqId, '[' + `{"seed":"${value}"},`.repeat(99) + `{"seed":"${value}"}]`);
}

// ---- POST /deserialization -------------------------------------------------
const JSON_TYPES = new Set(['application/json', 'text/x-json', 'application/jsonrequest']);

function mediaType(contentType) {
  if (!contentType) return '';
  const semi = contentType.indexOf(';');
  return (semi === -1 ? contentType : contentType.slice(0, semi)).trim().toLowerCase();
}

const isJsonType = type => JSON_TYPES.has(type) || (type.startsWith('application/') && type.endsWith('+json'));

function deserialization(reqId, query) {
  if (!isJsonType(mediaType(engine.getHeader(reqId, 'content-type')))) return status(reqId, 415);
  if (query) return status(reqId, 400);
  const raw = engine.getBody(reqId);
  if (!raw || raw.byteLength === 0) return status(reqId, 400);
  let doc;
  try {
    doc = JSON.parse(Buffer.from(raw).toString('utf8'));
  } catch {
    return status(reqId, 400);
  }
  if (doc === null || typeof doc !== 'object' || Array.isArray(doc)) return status(reqId, 400);
  const items = doc.items;
  if (!Array.isArray(items) || items.length !== 100) return status(reqId, 400);
  let checksum = 0;
  for (let i = 0; i < 100; i++) {
    const item = items[i];
    if (item === null || typeof item !== 'object' || Array.isArray(item)) return status(reqId, 400);
    const id = item.id;
    // Number.isInteger accepts 2.0 and 2e0 (they are the integer 2) and
    // rejects 1.5, strings, booleans, null, arrays and objects.
    if (!Number.isInteger(id) || id < 0 || id > 1000000) return status(reqId, 400);
    checksum += id;
  }
  json(reqId, `{"count":100,"checksum":${checksum}}`);
}

// ---- POST /upload ------------------------------------------------------------
// multipart/form-data per RFC 7578 over RFC 2046 framing: parts delimited by
// CRLF "--" boundary, headers ended by an empty line, the body closed by
// "--" boundary "--". Exactly one part named `file` that is an uploaded file
// (has a filename); parts with filename="" are unselected inputs and are
// ignored; a part named `file` without a filename is a text field, not a file.
const CRLF = Buffer.from('\r\n');
const HEADER_END = Buffer.from('\r\n\r\n');

function boundaryOf(contentType) {
  const m = /;\s*boundary\s*=\s*(?:"([^"]*)"|([^;]+))/i.exec(contentType);
  if (!m) return null;
  const b = (m[1] ?? m[2] ?? '').trim();
  return b || null;
}

function dispositionParams(headers) {
  // Content-Disposition: form-data; name="file"; filename="payload.bin"
  const m = /^content-disposition:[ \t]*([^\r\n]*)/im.exec(headers);
  if (!m) return null;
  const params = new Map();
  for (const piece of m[1].split(';').slice(1)) {
    const eq = piece.indexOf('=');
    if (eq === -1) continue;
    const key = piece.slice(0, eq).trim().toLowerCase();
    let value = piece.slice(eq + 1).trim();
    if (value.startsWith('"') && value.endsWith('"') && value.length >= 2) value = value.slice(1, -1);
    if (!params.has(key)) params.set(key, value);
  }
  return params;
}

// Returns the parts as [{ name, filename, data }] or null when the body is
// not a well-formed multipart message for this boundary.
function parseMultipart(body, boundary) {
  const delimiter = Buffer.from(`--${boundary}`);
  const parts = [];
  // The first delimiter may stand at the very start (no preamble).
  let pos = body.indexOf(delimiter);
  if (pos === -1) return null;
  pos += delimiter.length;
  for (;;) {
    if (body.length < pos + 2) return null;
    if (body[pos] === 0x2d && body[pos + 1] === 0x2d) return parts; // "--": the closing delimiter
    if (body[pos] !== 0x0d || body[pos + 1] !== 0x0a) return null; // delimiter must end with CRLF
    pos += 2;
    const headEnd = body.indexOf(HEADER_END, pos);
    if (headEnd === -1) return null;
    const headers = body.toString('latin1', pos, headEnd);
    const params = dispositionParams(headers);
    if (!params || !params.has('name')) return null;
    const dataStart = headEnd + HEADER_END.length;
    // The part's data runs to the CRLF that precedes the next delimiter.
    let next = body.indexOf(delimiter, dataStart);
    for (;;) {
      if (next === -1) return null;
      if (next >= dataStart + 2 && body[next - 2] === 0x0d && body[next - 1] === 0x0a) break;
      next = body.indexOf(delimiter, next + 1); // boundary-like bytes inside the data
    }
    parts.push({ name: params.get('name'), filename: params.get('filename'), data: body.subarray(dataStart, next - 2) });
    pos = next + delimiter.length;
  }
}

function upload(reqId) {
  const contentType = engine.getHeader(reqId, 'content-type') || '';
  if (mediaType(contentType) !== 'multipart/form-data') return status(reqId, 415);
  const boundary = boundaryOf(contentType);
  if (!boundary) return status(reqId, 400);
  const raw = engine.getBody(reqId);
  if (!raw || raw.byteLength === 0) return status(reqId, 400);
  const parts = parseMultipart(Buffer.from(raw), boundary);
  if (!parts) return status(reqId, 400);
  let file = null;
  for (const part of parts) {
    if (part.name !== 'file') continue; // other fields may be ignored
    if (part.filename === undefined) return status(reqId, 400); // a text field named file
    if (part.filename === '') continue; // an unselected file input
    if (file !== null) return status(reqId, 400); // repeated uploads
    file = part.data;
  }
  if (file === null) return status(reqId, 400);
  const first = file.length === 0 ? 'null' : String(file[0]);
  const last = file.length === 0 ? 'null' : String(file[file.length - 1]);
  json(reqId, `{"first_byte":${first},"last_byte":${last},"checksum":"${hash('sha256', file)}"}`);
}

// ---- dispatch -----------------------------------------------------------------
const server = engine.serve(
  {
    onRequest(reqId, methodIdx, path) {
      if (methodIdx === GET) {
        if (path.startsWith(USER_PREFIX)) {
          const id = path.slice(USER_PREFIX.length);
          if (id.length > 0 && !id.includes('/')) return user(reqId, id);
        } else if (path === '/serialization') {
          return serialization(reqId, engine.getQuery(reqId));
        }
      } else if (methodIdx === POST) {
        if (path === '/deserialization') return deserialization(reqId, engine.getQuery(reqId));
        if (path === '/upload') return upload(reqId);
      }
      status(reqId, 404);
    },
    onAborted() {},
    onWritable() {},
  },
  {
    // Every cluster worker binds the port itself (SO_REUSEPORT).
    reusePort: true,
    // The load script uploads a 256 KiB file plus multipart framing; the
    // engine's default cap is 10 MiB, kept explicit here.
    maxBodySize: 10 * 1024 * 1024,
  }
);

// GET "/" and POST "/user" => 200 with an empty body. Fixed replies are the
// engine's static routes: answered inside the engine, no JS per request.
engine.setStaticRoute(server, GET, '/', 200, null, '');
engine.setStaticRoute(server, POST, '/user', 200, null, '');

// Start the server on port 3000
engine.listen(server, '0.0.0.0', 3000);
