# nim r -d:release -d:threadsafe server

import guildenstern/[dispatcher, httpserver]

const ThreadCount = 100

proc handle() =
  try:
    if not isMethod("GET") or not startsUri("/user/"): reply(Http200)
    else:
      let id = getUri()[6 .. ^1]
      reply(id)
  except: reply(Http500)
  
proc run() =
  let server = newHttpServer(handle, NONE, true, NoBody)
  server.start(3000, ThreadCount, ThreadCount)
  joinThread(server.thread)

run()

