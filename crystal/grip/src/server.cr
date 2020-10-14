require "grip"

Grip.config do |cfg|
  cfg.env = "production"
end

class Index < Grip::Controllers::Http
  def get(context)
    text!(context, "")
  end
end

class Users < Grip::Controllers::Http
  def get(context)
    params = url?(context)
    text!(context, params["id"])
  end
end

class User < Grip::Controllers::Http
  def post(context)
    text!(context, "")
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
