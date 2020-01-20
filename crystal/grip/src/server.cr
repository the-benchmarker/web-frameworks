require "grip"

Grip.config do |cfg|
  cfg.env = "production"
  cfg.logging = false
end

class Index < Grip::HttpConsumer
  route "/", ["GET"]

  def get(env)
    headers(env, {"Content-Type" => "text/html"})
    {"status" => 200, "content" => nil}
  end
end

class Users < Grip::HttpConsumer
  route "/user/:id", ["GET"]

  def get(env)
    headers(env, {"Content-Type" => "text/html"})
    {"status" => 200, "content" => url(env)["id"]}
  end
end

class User < Grip::HttpConsumer
  route "/user", ["POST"]

  def post(env)
    headers(env, {"Content-Type" => "text/html"})
    {"status" => 200, "content" => nil}
  end
end

Grip.config.add_router Grip::HttpRouteHandler::INSTANCE
add_handlers [Index, Users, User]

System.cpu_count.times do |i|
  Process.fork do
    Grip.run do |config|
      server = config.server.not_nil!
      server.bind_tcp "0.0.0.0", 3000, reuse_port: true
    end
  end
end

sleep
