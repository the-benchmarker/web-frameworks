class Home::Index < Lucky::Action
  accepted_formats [:plain_text], default: :plain_text

  get "/" do
    plain_text ""
  end
end

class Users::Create < Lucky::Action
  accepted_formats [:plain_text], default: :plain_text

  post "/user" do
    plain_text ""
  end
end

class Users::Show < Lucky::Action
  accepted_formats [:plain_text], default: :plain_text

  get "/user/:id" do
    plain_text id
  end
end
