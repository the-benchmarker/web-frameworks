require "shivneri"
require "./routes"
include Server

module Server
  VERSION = "0.1.0"
  Shivneri.port = 3000
  Shivneri.host = "0.0.0.0"
  Shivneri.open  
end
