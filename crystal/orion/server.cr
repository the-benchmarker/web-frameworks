require "orion"

router MyApplication do

  get "/", ->(context : Context) do
    context.response.puts ""
  end

  get "/user/:id", ->(context : Context) do
    context.response.puts context.request.path_params["id"]
  end

  post "/user", ->(context : Context) do
    context.response.puts ""
  end

end

puts MyApplication.visualize

MyApplication.listen(host: "0.0.0.0", port: 3000, reuse_port: true)
