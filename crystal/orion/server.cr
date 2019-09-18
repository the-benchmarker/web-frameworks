require "orion"

router MyApplication do
  get "/", ->(context : Context) do
    context.response.print ""
  end

  get "/user/:id", ->(context : Context) do
    context.response.print context.request.path_params["id"]
  end

  post "/user", ->(context : Context) do
    context.response.print ""
  end
end

puts MyApplication.visualize

System.cpu_count.times do |i|
  Process.fork do
    MyApplication.listen(host: "0.0.0.0", port: 3000, reuse_port: true)
  end
end

sleep
