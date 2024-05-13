# Package

description = "Macro-oriented asynchronous web-framework written with ♥"
author = "HapticX"
version = "3.11.0"
license = "MIT"
srcDir = "src"
installExt = @["nim"]
bin = @["hpx"]

# Dependencies

requires "happyx >= 3.11 & < 3.12"
requires "cligen"
requires "checksums"
requires "regex"
requires "httpx"
requires "microasynchttpserver"
requires "httpbeast"
requires "illwill"
requires "nimja"
requires "websocket"
requires "websocketx"
requires "nimpy"
