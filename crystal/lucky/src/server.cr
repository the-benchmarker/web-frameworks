require "./app"

host = Lucky::Server.settings.host
port = Lucky::Server.settings.port

server = HTTP::Server.new([
  Lucky::HttpMethodOverrideHandler.new,
  Lucky::ErrorHandler.new(action: Errors::Show),
  Lucky::RouteHandler.new,
])

System.cpu_count.times do |i|
  Process.fork do
    server.bind_tcp host, port, reuse_port: true
    server.listen
  end
end
