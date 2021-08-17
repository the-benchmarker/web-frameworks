# disable logging
require "log"
Log.setup(:none)

require "router"

struct Server
  def draw_routes
    get "/" do |context, params|
      context.response.status_code = 200
      context
    end

    get "/user/:id" do |context, params|
      context.response.status_code = 200
      context.response.print params["id"].to_s
      context
    end

    post "/user" do |context, params|
      context.response.status_code = 200
      context
    end
  end

  def initialize
    draw_routes
  end

  def run
    server = HTTP::Server.new(route_handler)
    server.bind_tcp("0.0.0.0", 3000, true)
    server.listen
  end

  include Router
end

System.cpu_count.times do |i|
  Process.fork do
    server = Server.new
    server.run
  end
end

sleep
