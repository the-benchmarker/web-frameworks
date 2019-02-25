require "onyx-rest"

router = Onyx::HTTP::Router.new do
  get "/" { }

  get "/user/:id" do |env|
    env.response.output << env.request.path_params["id"]
  end

  post "/user" { }
end

server = Onyx::HTTP::Server.new(router)

(System.cpu_count - 1).times do |i|
  Process.fork do
    server.bind_tcp("0.0.0.0", 3000, reuse_port: true)
    server.listen
  end
end

server.bind_tcp("0.0.0.0", 3000, reuse_port: true)
server.listen
