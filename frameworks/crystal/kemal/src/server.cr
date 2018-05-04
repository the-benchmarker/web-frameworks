require "kemal"

Kemal.config do |cfg|
	cfg.serve_static = false
	cfg.logging = false
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
Kemal.run
