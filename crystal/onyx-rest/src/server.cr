require "onyx-rest"

router = Onyx::REST::Router.new do
  get "/" { }

  get "/user/:id" do |env|
    env.request.path_params["id"]
  end

  post "/user" { }
end

server = Onyx::REST::Server.new(router)

(System.cpu_count - 1).times do |i|
  Process.fork do
    server.bind_tcp("0.0.0.0", 3000, reuse_port: true)
    server.listen
  end
end

server.bind_tcp("0.0.0.0", 3000, reuse_port: true)
server.listen
