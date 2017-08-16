require "raze"

get "/" do |ctx|
  nil
end

get "/user/:id" do |env|
  ctx.params["id"]
end

post "/user" do |ctx|
  nil
end

Raze.run
