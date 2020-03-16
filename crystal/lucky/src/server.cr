require "./app"

host = Lucky::Server.settings.host
port = Lucky::Server.settings.port

server = HTTP::Server.new([
  Lucky::HttpMethodOverrideHandler.new,
  Lucky::RouteHandler.new,
])

System.cpu_count.times do |i|
  Process.fork do
    server.listen host, port, reuse_port: true
  end
end

sleep
