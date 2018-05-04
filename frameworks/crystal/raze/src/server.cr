require "raze"

get "/" do |ctx|
  nil
end

get "/user/:id" do |ctx|
  ctx.params["id"].as(String)
end

post "/user" do |ctx|
  nil
end

Raze.config.port = 3000
Raze.config.env = "production"
Raze.run
