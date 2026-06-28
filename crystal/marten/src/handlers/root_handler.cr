class RootHandler < Marten::Handler
  # GET / - Root endpoint
  def get
    # Health check and root endpoint
    head(200)
  end
end
