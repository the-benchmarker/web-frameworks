# Package

version       = "0.1.0"
author        = "Anonymous"
description   = "A new awesome baspolato package"
license       = "MIT"
srcDir        = "."
bin           = @["server"]

backend       = "c"

# Dependencies

requires "nim >= 1.2.4"
requires "https://github.com/itsumura-h/nim-allographer#head"
requires "https://github.com/itsumura-h/nim-basolato >= 0.5.4"
