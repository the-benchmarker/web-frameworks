require "runcobo"

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
  query NamedTuple(id: Int32)

  call do
pp params
    render_plain params[:id].to_s
  end
end

Runcobo.start
