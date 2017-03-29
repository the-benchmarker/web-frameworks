require "route"

class Routecr

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
    route_log_level(Production)

    # routing
    get  "/",         @index
    get  "/user/:id", @user
    post "/user",     @register_user
  end

  def run
    server = HTTP::Server.new(3000, routeHandler)
    server.listen
  end

  include Route
end

routecr = Routecr.new
routecr.run
