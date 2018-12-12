class Welcome < Application
  base "/"

  def index
    head :ok
  end
end
