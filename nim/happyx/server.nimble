# Package

description = "Asynchronous web-framework written with ♥"
author = "HapticX"
version = "0.17.2"
license = "GNU GPLv3"
srcDir = "src"
installExt = @["nim"]
bin = @["hpx"]

# Dependencies

requires "happyx >= 0.17 & < 0.18"
requires "cligen"
requires "regex"
requires "httpx"
requires "illwill"
requires "nimja"
requires "websocketx"
