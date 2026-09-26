# Package

version = "0.1.0"
author = "Ryan Walklin"
description = "hashi implementation"
license = "MIT"

# Dependencies
# hashi is not in the Nim package list (it targets Nimony, not Nim 2), so it
# is required by URL at a release tag. pnak, Nimony's package manager, reads
# this file and writes the module search path.

requires "https://github.com/kaitakeradiology/hashi#v0.1.2"
