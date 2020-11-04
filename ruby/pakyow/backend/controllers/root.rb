# frozen_string_literal: true

controller do
  disable_protection :csrf

  default do
    # do nothing
  end

  get '/user/:id' do
    send params[:id].to_s
  end

  post '/user' do
    # do nothing
  end
end
