require "shivneri"
require "./routes"

# include App

module App
  VERSION = "0.1.0"
  Shivneri.port = 3000
  Shivneri.open do
    puts "Shivneri is created"
  end
end
