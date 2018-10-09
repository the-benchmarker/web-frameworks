require "atom-web"

struct UserAction
  include Atom::Action

  params do
    type id : Int32
  end

  def call
    text(id)
  end
end

router = Atom::Handlers::Router.new do
  get "/"
  post "/user"
  get "/user/:id", UserAction
end

logger = Logger.new(STDOUT, Logger::DEBUG)
request_logger = Atom::Handlers::RequestLogger.new(logger)

server = HTTP::Server.new([request_logger, router]) do |context|
  if proc = context.proc
    proc.call(context)
  else
    context.response.respond_with_error("Not Found: #{context.request.path}", 404)
  end
rescue ex : Params::Error
  context.response.respond_with_error(ex.message, 400)
end

server.bind_tcp("0.0.0.0", 3000, reuse_port: true)
server.listen
