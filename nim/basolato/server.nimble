# Package

version       = "0.1.0"
author        = "Anonymous"
description   = "A new awesome basolato package"
license       = "MIT"
srcDir        = "."
bin           = @["server"]

backend       = "c"

# Dependencies

requires "nim >= 1.2.4"
requires "https://github.com/itsumura-h/nim-basolato >= 0.8.0 & < 0.9.0"
