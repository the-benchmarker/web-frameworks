const Server = require("@pxe/server");

const regx = /^\/user\/(?<id>[0-9]+)/i;

const app = new Server();

app.use(async ctx => {
    const url = ctx.request.url;
    const res = ctx.response;
    const req = ctx.request;
    
    ctx.options.finishResponse = () => res.raw.end(res.body || "");

    if ((req.method === "POST" && url === "/user") || (req.method === "GET" && url === "/"))
        return;
     
    const id = regx.exec(url);
    if (id) {
        ctx.response.body = id.groups.id;
        return;
    }
    
    ctx.response.raw.statusCode = 404;
});

app.ls(3000);