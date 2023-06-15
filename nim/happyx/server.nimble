# Package

description = "Macro-oriented asynchronous web-framework written with ♥"
author = "HapticX"
version = "1.3.0"
license = "GNU GPLv3"
srcDir = "src"
installExt = @["nim"]
bin = @["hpx"]

# Dependencies

requires "happyx >= 1.3 & < 1.4"
requires "cligen"
requires "regex"
requires "httpx"
requires "illwill"
requires "nimja"
requires "websocketx"
