require "http"
require "json"
require "uri"
require "tempfile"
require "radix"

require "./raze/*"

module Raze
  def self.run(port = Raze.config.port)
    config = Raze.config
    config.global_handlers << Raze::ExceptionHandler::INSTANCE
    Raze::StaticFileHandler::INSTANCE.public_dir = config.static_dir
    Raze.config.dynamic_static_paths << "/" unless config.static_indexing
    config.global_handlers << Raze::StaticFileHandler::INSTANCE
    config.global_handlers << Raze::WebSocketServerHandler::INSTANCE
    config.global_handlers << Raze::ServerHandler::INSTANCE
    config.setup

    unless Raze.config.error_handlers.has_key?(404)
      error 404 do |ctx|
        unless ctx.response.headers.has_key?("Content-Type")
          ctx.response.content_type = "text/html"
        end
        ctx.response.status_code = 404
        "Not Found"
      end
    end

    unless Raze.config.error_handlers.has_key?(500)
      error 500 do |ctx, ex|
        unless ctx.response.headers.has_key?("Content-Type")
          ctx.response.content_type = "text/html"
        end
        ctx.response.status_code = 500
        Raze.config.env == "development" ? ex.message : "An error ocurred"
      end
    end

    server = HTTP::Server.new(config.host, config.port, config.global_handlers)

    # tls/ssl if a key and a cert are added to config
    if config.tls_key && config.tls_cert
      tls_context = OpenSSL::SSL::Context::Server.new
      tls_context.private_key = config.tls_key.as(String)
      tls_context.certificate_chain = config.tls_cert.as(String)
      server.tls = tls_context
    end

    puts "\nlistening at localhost:" + config.port.to_s if config.logging
    server.listen
  end
end
