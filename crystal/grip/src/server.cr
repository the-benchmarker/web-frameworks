require "grip"

Grip.config do |cfg|
  cfg.env = "production"
  cfg.logging = false
end


class IndexHttpConsumer < Grip::HttpConsumer
  def get(req)
    html(
      nil
    )
  end
end

class UsersHttpConsumer < Grip::HttpConsumer
  def get(req)
    html(
      url["id"]
    )
  end
end

class UserHttpConsumer < Grip::HttpConsumer
  def post(req)
    html(
      nil
    )
  end
end

class Api < Grip::Application
  scope do
    get "/", IndexHttpConsumer
    get "/user/:id", UsersHttpConsumer
    post "/user", UserHttpConsumer
  end
end

api = Api.new

System.cpu_count.times do |_|
  Process.fork do
    api.run do |config|
      server = config.server.not_nil!
      server.bind_tcp "0.0.0.0", 3000, reuse_port: true
    end
  end
end

sleep
