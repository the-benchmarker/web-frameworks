class UserDetailHandler < Marten::Handler
  # GET /user/:id - User detail endpoint
  def get
    user_id = params["id"].to_s
    
    # Input validation
    if user_id.empty?
      respond("Invalid user ID", status: 400, content_type: "text/plain")
    else
      respond(user_id, content_type: "text/plain")
    end
  end
end
