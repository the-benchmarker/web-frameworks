# Package

description = "Macro-oriented asynchronous web-framework written with ♥"
author = "HapticX"
version = "2.5.1"
license = "MIT"
srcDir = "src"
installExt = @["nim"]
bin = @["hpx"]

# Dependencies

requires "happyx >= 2.7 & < 2.8"
requires "cligen"
requires "regex"
requires "httpx"
requires "microasynchttpserver"
requires "httpbeast"
requires "illwill"
requires "nimja"
requires "websocket"
requires "websocketx"
requires "nimpy"
