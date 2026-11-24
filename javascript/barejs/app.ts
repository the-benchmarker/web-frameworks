import { createHash } from 'node:crypto';
import { BareJS, type Context } from 'barejs';

const ID = /^[0-9]{8}$/;
const SEED = /^[a-z]{8}$/;
const JSON_TYPE = /^(?:application\/(?:[^/;\s]+\+json|json|jsonrequest)|text\/x-json)$/i;
const MULTIPART_TYPE = /^multipart\/form-data$/i;

const mediaType = (header: string | null) => (header ?? '').split(';', 1)[0]!.trim();
const JSON_INIT = { status: 200, headers: { 'content-type': 'application/json' } };
const EMPTY_200 = { status: 200, headers: { 'content-type': 'text/plain' } };
const fail = (code: number) => new Response('', { status: code });
const json = (text: string) => new Response(text, JSON_INIT);

function seedOf(url: string): string | null {
  const at = url.indexOf('?');
  if (at < 0) return null;
  const pairs = [...new URLSearchParams(url.slice(at + 1))];
  return pairs.length === 1 && pairs[0]![0] === 'seed' && SEED.test(pairs[0]![1]) ? pairs[0]![1] : null;
}

const BOUNDARY = /^[0-9A-Za-z'()+_,\-./:=?]{1,70}$/;
const CRLF = Buffer.from('\r\n');
const HEADER_END = Buffer.from('\r\n\r\n');
const DISPOSITION = /^content-disposition:\s*form-data\s*(;.*)$/im;
const NAME = /;\s*name="([^"]*)"/;
const FILENAME = /;\s*filename="([^"]*)"/;

function boundaryOf(header: string): string | null {
  const params = header.split(';').slice(1);
  let boundary: string | null = null;
  for (const param of params) {
    const eq = param.indexOf('=');
    if (eq < 0) return null;
    if (param.slice(0, eq).trim().toLowerCase() !== 'boundary') continue;
    if (boundary !== null) return null;
    const raw = param.slice(eq + 1).trim();
    boundary = raw.length > 1 && raw[0] === '"' && raw.at(-1) === '"' ? raw.slice(1, -1) : raw;
  }
  return boundary !== null && BOUNDARY.test(boundary) ? boundary : null;
}

function uploadedFile(body: Buffer, boundary: string): Buffer | null {
  const open = Buffer.from(`--${boundary}`);
  const next = Buffer.from(`\r\n--${boundary}`);
  if (!body.subarray(0, open.length).equals(open)) return null;
  let at = open.length;
  let found: Buffer | null = null;
  for (;;) {
    if (body[at] === 45 && body[at + 1] === 45) return found ?? null;
    if (!body.subarray(at, at + 2).equals(CRLF)) return null;
    const headEnd = body.indexOf(HEADER_END, at);
    if (headEnd < 0) return null;
    const head = body.toString('latin1', at + 2, headEnd + 2);
    let end = body.indexOf(next, headEnd + 2);
    while (end >= 0) {
      const after = end + next.length;
      if ((body[after] === 45 && body[after + 1] === 45) || (body[after] === 13 && body[after + 1] === 10)) break;
      end = body.indexOf(next, end + 1);
    }
    if (end < 0) return null;
    const disposition = DISPOSITION.exec(head)?.[1];
    if (disposition === undefined) return null;
    if (NAME.exec(disposition)?.[1] === 'file') {
      const filename = FILENAME.exec(disposition)?.[1];
      if (filename === undefined) return null;
      if (filename !== '') {
        if (found !== null) return null;
        found = body.subarray(headEnd + 4, end);
      }
    }
    at = end + next.length;
  }
}

const app = new BareJS();

app.get('/', () => new Response('', EMPTY_200));

app.get('/user/:id', (ctx: Context) => {
  const rawId = ctx.params.id ?? '';
  let id: string;
  try {
    id = rawId.includes('%') ? decodeURIComponent(rawId) : rawId;
  } catch {
    return fail(400);
  }
  return ID.test(id) ? json(`{"id":"${id}"}`) : fail(400);
});

app.post('/user', () => new Response('', EMPTY_200));

app.get('/serialization', (ctx: Context) => {
  const seed = seedOf(ctx.req.url);
  if (seed === null) return fail(400);
  return json(`[${new Array(100).fill(`{"seed":"${seed}"}`).join(',')}]`);
});

app.post('/deserialization', async (ctx: Context) => {
  const req = ctx.req;
  if (!JSON_TYPE.test(mediaType(req.headers.get('content-type')))) return fail(415);
  if (req.url.includes('?')) return fail(400);
  let body: any;
  try {
    body = JSON.parse(await req.text());
  } catch {
    return fail(400);
  }
  const items = body !== null && typeof body === 'object' ? body.items : undefined;
  if (!Array.isArray(items) || items.length !== 100) return fail(400);
  let checksum = 0;
  for (const item of items) {
    const id = item !== null && typeof item === 'object' ? item.id : undefined;
    if (!Number.isInteger(id) || id < 0 || id > 1000000) return fail(400);
    checksum += id;
  }
  return json(`{"count":100,"checksum":${checksum}}`);
});

app.post('/upload', async (ctx: Context) => {
  const req = ctx.req;
  const type = req.headers.get('content-type') ?? '';
  if (!MULTIPART_TYPE.test(mediaType(type))) return fail(415);
  const boundary = boundaryOf(type);
  if (boundary === null) return fail(400);
  const file = uploadedFile(Buffer.from(await req.arrayBuffer()), boundary);
  if (file === null) return fail(400);
  const last = file.length - 1;
  const checksum = createHash('sha256').update(file).digest('hex');
  return json(
    `{"first_byte":${file.length ? file[0] : 'null'},"last_byte":${file.length ? file[last] : 'null'},"checksum":"${checksum}"}`,
  );
});

app.listen(3000, '0.0.0.0', true);
