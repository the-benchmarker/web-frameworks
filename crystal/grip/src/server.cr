require "grip"

class IndexController < Grip::Controllers::Http
  def get(context)
    context.text("")
  end
end

class UserController < Grip::Controllers::Http
  def get(context)
    id = context.fetch_path_params.["id"]

    context.text(id)
  end

  def post(context)
    context.text("")
  end
end

class Application < Grip::Application
  def reuse_port
    true
  end

  def port
    3000
  end

  def router : Array(HTTP::Handler)
    [http_handler] of HTTP::Handler
  end

  def server : HTTP::Server
    HTTP::Server.new(router)
  end

  def initialize
    super(environment: "production", serve_static: false)

    get "/", IndexController
    get "/user/:id", UserController
    post "/user", UserController
  end
end

app = Application.new

System.cpu_count.times do |_|
  Process.fork do
    app.run
  end
end

sleep
