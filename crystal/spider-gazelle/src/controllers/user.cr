class User < Application
  base "/user"

  get "/", :index do
    head :ok
  end

  get "/:id", :show do
    render text: route_params["id"]
  end

  post "/", :create do
    head :ok
  end
end
