import cgi, strtabs, httpcore, guildenstern/[ctxheader, ctxbody], strutils

proc handleGet(ctx: HttpCtx) =
  if ctx.getUri() == "/":
    ctx.reply("")
  else:
    let uri = ctx.getUri()
    let id = uri[6 .. ^1]

proc handlePost(ctx: HttpCtx) =
  ctx.reply("")

var server = new GuildenServer
server.initHeaderCtx(handleGet, 3000, false)
server.initBodyCtx(handlePost, 3000)

server.serve(loglevel = DEBUG)