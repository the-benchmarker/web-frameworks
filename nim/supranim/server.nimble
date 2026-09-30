# Package
version       = "0.1.0"
author        = "George Lemon"
description   = "A new awesome Supranim application"
license       = "MIT"
bin           = @["server"]

# Dependencies

requires "nim >= 2.0.0"
requires "supranim  >= 0.1.10[powpow]"
requires "emitter >= 0.2.1[powpow]"