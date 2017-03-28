require "route"

class Routecr

  def initialize
    route_log_level(Production)

    # routing
    get  "/", index
    get  "/user/:id", user
    post "/user", registerUser
  end

  def index(context : Context, uriParams : UriParams) : Context
    context.response.status_code = 200
    context
  end

  def user(context : Context, uriParams : UriParams) : Context
    context.response.status_code = 200
    context.response.print uriParams["id"]
    context
  end

  def registerUser(context : Context, uriParams : UriParams) : Context
    context.response.status_code = 200
    context
  end

  def run
    # run 4 servers concurrently
    spawn_server(4) do
      server = HTTP::Server.new(3000){ |context| routing(context) }
      server.listen(true) # reuse the port
    end
  end

  include Route
end

routecr = Routecr.new
routecr.run
