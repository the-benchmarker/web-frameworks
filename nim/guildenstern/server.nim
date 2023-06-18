import guildenstern/[ctxheader], uri

proc handleGet(ctx: HttpCtx) =
  if ctx.getMethod() == "GET":
    let uri = parseUri(ctx.getUri())
    if uri.path == "/":
      let r = ""
      ctx.reply(r)
    else:
      let id = uri.path[6 .. ^1]
      ctx.reply(id)
  else:
    let r = ""
    ctx.reply(r)

var server = new GuildenServer
server.initHeaderCtx(handleGet, 3000)

server.serve(loglevel = NONE)
