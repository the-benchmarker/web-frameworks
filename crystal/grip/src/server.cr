require "grip"

class IndexController < Grip::Controllers::Http
  def get(context)
    context
      .text("")
  end
end

class UserController < Grip::Controllers::Http
  def get(context)
    id = 
      context
        .fetch_path_params
        .["id"]
    
    context
      .text(id)
  end
  
  def post(context)
    context
      .text("")
  end
end

class Application < Grip::Application
  def reuse_port
    true
  end
  
  def routes
    get "/", IndexController
    get "/user/:id", UserController
    post "/user", UserController
  end
end

app = App.new

System.cpu_count.times do |_|
  Process.fork do
    app.run
  end
end

sleep
