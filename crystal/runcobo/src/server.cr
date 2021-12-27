require "runcobo"
ENV["SKIP_LOG"] = "true"

class Index < BaseAction
  get "/"

  call do
    render_plain ""
  end
end

class CreateUser < BaseAction
  post "/user"

  call do
    render_plain ""
  end
end

class ShowUser < BaseAction
  get "/user/:id"
  url NamedTuple(id: Int32)

  call do
    render_plain params[:id].to_s
  end
end

System.cpu_count.times do |_|
  Process.fork do
    Runcobo.start(reuse_port: true)
  end
end

sleep