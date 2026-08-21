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

Fiber::ExecutionContext.default.resize(maximum: Fiber::ExecutionContext.default_workers_count)
MyApplication.start(workers: System.cpu_count, host: "0.0.0.0", port: 3000, reuse_port: true)
