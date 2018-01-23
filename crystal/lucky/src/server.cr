require "./app"

host = Lucky::Server.settings.host
port = Lucky::Server.settings.port

server = HTTP::Server.new(host, port, [
  Lucky::HttpMethodOverrideHandler.new,
  Lucky::LogHandler.new,
  Lucky::SessionHandler.new,
  Lucky::Flash::Handler.new,
  Lucky::ErrorHandler.new(action: Errors::Show),
  Lucky::RouteHandler.new,
  Lucky::StaticFileHandler.new("./public", false),
])

puts "Listening on http://#{host}:#{port}"

server.listen
