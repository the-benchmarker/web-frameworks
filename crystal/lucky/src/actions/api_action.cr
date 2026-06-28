class Home::Index < Lucky::Action
  accepted_formats [:plain_text], default: :plain_text

  get "/" do
    # Root endpoint - empty response for benchmarking
    plain_text ""
  end
end

class Users::Create < Lucky::Action
  accepted_formats [:plain_text, :json], default: :plain_text

  post "/user" do
    # User creation endpoint
    # Security: Validate input and return appropriate status
    if params.valid?
      plain_text "", status: 201
    else
      plain_text "Invalid request", status: 400
    end
  end
end

class Users::Show < Lucky::Action
  accepted_formats [:plain_text, :json], default: :plain_text

  get "/user/:id" do
    # User detail endpoint
    # Security: Validate and sanitize ID parameter
    user_id = id.to_s
    
    if user_id.empty?
      plain_text "Invalid user ID", status: 400
    else
      plain_text user_id
    end
  end
end
