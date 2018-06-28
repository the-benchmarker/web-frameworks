require "kemal"

Kemal.config do |cfg|
	cfg.serve_static = false
	cfg.logging = false
	cfg.env = "production"
end

get "/" do |env|
  nil
end

get "/user/:id" do |env|
  env.params.url["id"]
end

post "/user" do |env|
  nil
end

Kemal.config.env = "production"
Kemal.run do |cfg|
  cfg.server.not_nil!.bind_tcp("0.0.0.0", 3000, reuse_port: true)
end
