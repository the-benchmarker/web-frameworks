require "grip"

Grip.config do |cfg|
  cfg.env = "production"
end

class Index < Grip::Controller::Http
  def get(context)
    context.response.content_type = "text/html"
    context.response.print(nil)
  end
end

class Users < Grip::Controller::Http
  def get(context)
    params = url(context)
    context.response.content_type = "text/html"
    context.response.print(params["id"])
  end
end

class User < Grip::Controller::Http
  def post(context)
    context.response.content_type = "text/html"
    context.response.print(nil)
  end
end

class App < Grip::Application
  def initialize
    get "/", Index
    get "/user/:id", Users
    post "/user", User
  end
end

app = App.new

System.cpu_count.times do |_|
  Process.fork do
    app.run do |config|
      server = config.server.not_nil!
      server.bind_tcp "0.0.0.0", 3000, reuse_port: true
    end
  end
end

sleep
