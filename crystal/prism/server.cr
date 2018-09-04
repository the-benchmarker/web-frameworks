require "prism"

struct UserAction
  include Prism::Action
  include Prism::Action::Params

  params do
    type id : Int32
  end

  def call
    text(params[:id])
  end
end

router = Prism::Router.new do
  get "/"
  post "/user"
  get "/user/:id", UserAction
end

server = Prism::Server.new([router])
server.bind_tcp("0.0.0.0", 3000, reuse_port: true)
server.listen
