# frozen_string_literal: true

require("rack/app")

class App < Rack::App
  get "" do
    ""
  end

  get "/user/:id" do
    params["id"]
  end

  post "/user" do
    ""
  end
end

# for more check out how-to
run(App)
