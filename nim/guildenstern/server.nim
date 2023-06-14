import cgi, strtabs, httpcore, guildenstern/[ctxheader, ctxbody], uri

proc handleGet(ctx: HttpCtx) =
  let uri = parseUri(ctx.getUri())
  if uri.path == "/":
    ctx.reply("")
  else:
    let id = uri.path[6 .. ^1]

proc handlePost(ctx: HttpCtx) =
  ctx.reply("")

var server = new GuildenServer
server.initHeaderCtx(handleGet, 3000, false)
server.initBodyCtx(handlePost, 3000)

server.serve(loglevel = NONE)
