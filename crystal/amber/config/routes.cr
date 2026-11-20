Amber::Server.configure do |app|
  pipeline :web do
  end

  routes :web do
    get "/", ApplicationController, :index
    get "/user/:id", ApplicationController, :get
    post "/user", ApplicationController, :create
  end
end
