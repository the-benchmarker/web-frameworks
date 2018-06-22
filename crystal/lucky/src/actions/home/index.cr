class Home::Index < ApiAction
  get "/" do
    head 200
  end
end

class User::Show < ApiAction
  get "/user/:id" do
    render_text id.to_s
  end
end

class User::Create < ApiAction
  post "/user" do
    head 200
  end
end
