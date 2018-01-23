require "./app"

host = Lucky::Server.settings.host
port = Lucky::Server.settings.port

server = HTTP::Server.new(host, port, [
  Lucky::HttpMethodOverrideHandler.new,
  Lucky::ErrorHandler.new(action: Errors::Show),
  Lucky::RouteHandler.new,
])

puts "Listening on http://#{host}:#{port}"

server.listen
