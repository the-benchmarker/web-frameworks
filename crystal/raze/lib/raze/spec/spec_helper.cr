require "spec"
require "../src/raze"

def create_request_and_return_io(handler, request)
  io = IO::Memory.new
  response = HTTP::Server::Response.new(io)
  context = HTTP::Server::Context.new(request, response)
  handler.call(context)
  response.close
  io.rewind
  io
end

def create_ws_request_and_return_io(handler, request)
  io = IO::Memory.new
  response = HTTP::Server::Response.new(io)
  context = HTTP::Server::Context.new(request, response)
  begin
    handler.call context
  rescue IO::Error
    # Raises because the IO::Memory is empty
  end
  response.close
  io
end

def call_request_on_app(request)
  io = IO::Memory.new
  response = HTTP::Server::Response.new(io)
  context = HTTP::Server::Context.new(request, response)
  main_handler = Raze::ServerHandler::INSTANCE
  main_handler.call context
  response.close
  io.rewind
  HTTP::Client::Response.from_io(io, decompress: false)
end

def reset_config
  Raze.config.host = "0.0.0.0"
  Raze.config.port = 7777
  Raze.config.env = "development"
  Raze.config.static_dir_listing = false
  Raze.config.compress = true
  Raze.config.static_indexing = true
  Raze.config.static_dir = "./static"
  Raze.config.dynamic_static_paths = [] of String
  Raze.config.logging = true
  Raze.config.global_handlers = [] of HTTP::Handler
  Raze.config.error_handlers = {} of Int32 => HTTP::Server::Context, Exception -> String
  Raze.config.tls_key = nil
  Raze.config.tls_cert = nil
end

Spec.after_each do
  Raze::ServerHandler::INSTANCE.clear_tree
  reset_config
end
