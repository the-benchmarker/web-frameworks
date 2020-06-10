# Standard Library Imports
import std/sugar

# External Imports
import whip

let w = initWhip()

w.onGet "/", (w:Wreq) => w.send("")
w.onGet "/user/{id}", (w:Wreq) => w.send(w.path("id"))
w.onPost "/user", (w:Wreq) => w.send("")

w.start(3000)

