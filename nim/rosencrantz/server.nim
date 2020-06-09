# Standard Library Imports
import std/asynchttpserver
import std/asyncdispatch
# External Imports
import rosencrantz

let handler = get[
  path("/")[
    ok("")
  ] ~
  pathChunk("/user")[
    segment(proc(id: string): auto =
      ok($id)
    )
  ]
] ~ post[
  path("/user")[
    ok("")
  ]
]

let server = newAsyncHttpServer()

waitFor server.serve(Port(8080), handler)