require "router"

class Server

  @route_handler = RouteHandler.new

  @index = API.new do |context|
    context.response.status_code = 200
    context
  end

  @user = API.new do |context, params|
    context.response.status_code = 200
    context.response.print params["id"].to_s
    context
  end

  @register_user = API.new do |context|
    context.response.status_code = 200
    context
  end

  def initialize
    draw(@route_handler) do
      get  "/",         @index
      get  "/user/:id", @user
      post "/user",     @register_user
    end
  end

  def run
    server = HTTP::Server.new(3000, @route_handler)
    server.listen
  end

  include Router
end

server = Server.new
server.run
