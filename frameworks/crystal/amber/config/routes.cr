Amber::Server.configure do |app|
  routes :web do
    get "/", ApplicationController, :index
    get "/user/:id", ApplicationController, :get
    post "/user", ApplicationController, :create
  end
end
