class UserDetailHandler < Marten::Handler
  def get
    respond(params["id"].to_s, content_type: "text/plain")
  end
end
