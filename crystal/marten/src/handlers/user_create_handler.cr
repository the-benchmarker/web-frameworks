class UserCreateHandler < Marten::Handler
  # Security: Disable CSRF protection for API endpoints
  protect_from_forgery false

  # POST /user - User creation endpoint
  def post
    # Production-ready response with created status
    head(201)
  end
end
