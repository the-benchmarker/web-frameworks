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
end

app = Application.new(environment: "production")
app.run
