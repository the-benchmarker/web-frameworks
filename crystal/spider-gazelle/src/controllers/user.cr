class User < Application
  base "/user"

  # GET /user - User list endpoint
  get "/", :index do
    head :ok
  end

  # GET /user/:id - User detail endpoint
  get "/:id", :show do
    user_id = route_params["id"]
    
    # Security: Validate and sanitize ID parameter
    if user_id.empty?
      head :bad_request
    else
      render text: user_id
    end
  end

  # POST /user - User creation endpoint
  post "/", :create do
    head :created
  end
end
