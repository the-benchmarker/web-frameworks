import hashi

logLevel = error

proc root(req: Request): Response {.nimcall, raises.} =
  newResponse(200, "")

proc userById(req: Request): Response {.nimcall, raises.} =
  newResponse(200, pathParam(req, "id"))

proc createUser(req: Request): Response {.nimcall, raises.} =
  newResponse(200, "")

get("/", root)
get("/user/:id", userById)
post("/user", createUser)

serve(3000'u16)
