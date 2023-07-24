# Package

description = "Macro-oriented asynchronous web-framework written with ♥"
author = "HapticX"
version = "1.10.0"
license = "MIT"
srcDir = "src"
installExt = @["nim"]
bin = @["hpx"]

# Dependencies

requires "happyx >= 1.10 & < 1.11"
requires "cligen"
requires "regex"
requires "httpx"
requires "microasynchttpserver"
requires "httpbeast"
requires "illwill"
requires "nimja"
requires "websocket"
requires "websocketx"
