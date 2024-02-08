class UserCreateHandler < Marten::Handler
  protect_from_forgery false

  def post
    head(200)
  end
end
