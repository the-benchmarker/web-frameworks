require "kemal"

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
