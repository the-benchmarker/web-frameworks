require "grip"

class IndexController
  include Grip::Controllers::HTTP

  def get(context : Context) : Context
    context.put_status(200).text("").halt()
  end
end
class UserController
  include Grip::Controllers::HTTP

  def get(context : Context) : Context
 id = context.fetch_path_params.["id"]
  context.text(id).halt()
 end
def post(context : Context): Context
    context.put_status(200).text("").halt()
end
end

class Application
  include Grip::Application

  def initialize
get "/", IndexController
    get "/user/:id", UserController
    post "/user", UserController
  end
end

app = Application.new
app.run
