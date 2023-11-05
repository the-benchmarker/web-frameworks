const Server = require('@pxe/server');

const regx = /^\/user\/(?<id>[0-9]+)/i;

const app = new Server();

app.on('finish', (ctx) => {
  const res = ctx.response;

  return res.raw.end(res.body || '');
});

app.use(async (ctx) => {
  const url = ctx.request.url;
  const req = ctx.request;

  if ((req.method === 'POST' && url === '/user') || (req.method === 'GET' && url === '/')) return;

  const id = regx.exec(url);
  if (id) return (ctx.response.body = id.groups.id);

  ctx.response.raw.statusCode = 404;
});

app.ls(3000);
