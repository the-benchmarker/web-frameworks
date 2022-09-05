require "athena-routing"

handler = ART::RoutingHandler.new

handler.add "root", ART::Route.new("/", methods: "GET") do
end

handler.add "user", ART::Route.new("/user/{id<\\d+>}", methods: "GET") do |ctx, params|
  ctx.response << params["id"].not_nil!.to_i
end

handler.add "new_user", ART::Route.new("/user", methods: "POST") do
end

System.cpu_count.times do
  Process.fork do
    server = HTTP::Server.new([
      handler.compile,
    ])

    server.bind_tcp host: "0.0.0.0", port: 3000, reuse_port: true
    server.listen
  end
end

sleep
