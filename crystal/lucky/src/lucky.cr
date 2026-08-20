require "lucky"
require "../config/setup"
require "./actions/api_action"

host = Lucky::Server.settings.host
port = Lucky::Server.settings.port

server = HTTP::Server.new([
  Lucky::RouteHandler.new,
])

Fiber::ExecutionContext.default.resize(
  maximum: Fiber::ExecutionContext.default_workers_count,
)
server.listen host, port, reuse_port: true
