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
  env.params.url["id"]
end

post "/user" do |env|
  nil
end

Kemal.run
