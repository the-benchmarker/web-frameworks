# frozen_string_literal: true

require("hanami/api")

class App < Hanami::API
  get "/", to: ->(*) { [200, {}, [""]] }

  get "/user/:id" do
    params[:id]
  end

  post "/user" do
    ""
  end
end
