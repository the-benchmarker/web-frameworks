class Welcome < Application
  base "/"

  get "/", :index do
    head :ok
  end
end
