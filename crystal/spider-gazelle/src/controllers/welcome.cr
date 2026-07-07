class Welcome < Application
  base "/"

  # GET / - Root endpoint
  get "/", :index do
    # Production-ready empty response
    head :ok
  end
end
