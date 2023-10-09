# Package

description = "Macro-oriented asynchronous web-framework written with ♥"
author = "HapticX"
version = "2.5.1"
license = "MIT"
srcDir = "src"
installExt = @["nim"]
bin = @["hpx"]

# Dependencies

requires "happyx >= 3.0 & < 3.1"
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
