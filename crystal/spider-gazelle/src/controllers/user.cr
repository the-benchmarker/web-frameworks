class User < Application
  def index
    head :ok
  end

  def show
    render text: route_params["id"]
  end

  def create
    head :ok
  end
end
