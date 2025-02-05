require "grip"

class IndexController < Grip::Controllers::Http
  def get(context : Context) : Context
    context.put_status(200).text("").halt()
  end
end

class UserController < Grip::Controllers::Http
  def get(context : Context) : Context
    id = context.fetch_path_params.["id"]

    context.text(id).halt()
  end
  
  def post(context : Context) : Context
    context.put_status(200).text("").halt()
  end
end

class Application < Grip::Application
  def initialize(environment : String)
    # By default the environment is set to "development".
    super(environment)

    get "/", IndexController
    get "/user/:id", UserController
    post "/user", UserController

    # Enable request/response logging.
    router.insert(0, Grip::Handlers::Log.new)
  end

  def router : Array(HTTP::Handler)
    [@http_handler] of HTTP::Handler
  end

  def port : Int32
    3000
  end

  def reuse_port : Bool
    true
  end
end

app = Application.new(environment: "production")

System.cpu_count.times do |i|
  Process.fork do
    app.run
  end
end

sleep