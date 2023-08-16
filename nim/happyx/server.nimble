# Package

description = "Macro-oriented asynchronous web-framework written with ♥"
author = "HapticX"
version = "2.2.4"
license = "MIT"
srcDir = "src"
installExt = @["nim"]
bin = @["hpx"]

# Dependencies

requires "happyx >= 2.2 & < 2.3"
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
