require "runcobo"
ENV["SKIP_LOG"] = "true"

class Home::Index < BaseAction
  get "/"

  call do
    render_plain ""
  end
end

class Users::CreateUser < BaseAction
  post "/user"

  call do
    render_plain ""
  end
end

class Users::ShowUser < BaseAction
  get "/user/:id"
  url NamedTuple(id: Int32)

  call do
    render_plain params[:id].to_s
  end
end

Runcobo.start(reuse_port: true)
