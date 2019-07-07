require "kemal"

Kemal.config do |cfg|
  cfg.env = "production"
  cfg.serve_static = false
  cfg.logging = false
end

get "/" do |env|
  nil
end

get "/user/:id" do |env|
  id = env.params.url["id"]
  raise Kemal::Exceptions::RouteNotFound.new(env) unless /\d+/.match(id)
  id
end

post "/user" do |env|
  nil
end

System.cpu_count.times do |i|
  Process.fork do
    Kemal.run do |config|
      server = config.server.not_nil!
      server.bind_tcp "0.0.0.0", 3000, reuse_port: true
    end
  end
end

sleep
