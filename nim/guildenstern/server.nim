import guildenstern/[dispatcher, httpserver]

const ThreadCount = 100

proc handle() =
  try:
    if not isMethod("GET") or not startsUri("/user/"): reply(Http200)
    else:
      let id = getUri()[6 .. ^1]
      reply(id)
  except: reply(Http500)
  
let server = newHttpServer(handle, loglevel = NONE, contenttype = NoBody)
if not dispatcher.start(server, 3000, ThreadCount, ThreadCount): quit()
joinThread(server.thread)
