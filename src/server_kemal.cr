require "kemal"

get "/" do |env|
  #env.response.status_code = 200
  nil
end

get "/user/:id" do |env|
  #env.response.status_code = 200
  env.params.url["id"]
end

post "/user" do |env|
  #env.response.status_code = 200
  nil
end

Kemal.config do |config|
  config.logging = false
end

Kemal.run
