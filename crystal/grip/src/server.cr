require "grip"

class IndexHttpConsumer < Grip::HttpConsumer
  def get(req)
    headers("Content-Type", "text/html")
    req.response.print(nil)
  end
end

class UsersHttpConsumer < Grip::HttpConsumer
  def get(req)
    headers("Content-Type", "text/html")
    req.response.print(url["id"])
  end
end

class UserHttpConsumer < Grip::HttpConsumer
  def post(req)
    headers("Content-Type", "text/html")
    req.response.print(nil)
  end
end

get "/", IndexHttpConsumer
get "/user/:id", UsersHttpConsumer
post "/user", UserHttpConsumer

Grip.config do |cfg|
  cfg.env = "production"
  cfg.logging = false
end

System.cpu_count.times do |_|
  Process.fork do
    Grip.run do |config|
      server = config.server.not_nil!
      server.bind_tcp "0.0.0.0", 3000, reuse_port: true
    end
  end
end

sleep
