Bundler.require :default

module Bench
  class BaseAPI < Grape::API
    get '' do
      ''
    end
  end

  class UserAPI < Grape::API
    get '/user/:id' do
      params[:id]
    end
    post '/user' do
      ''
    end
  end

  class API < Grape::API
    mount ::Bench::BaseAPI
    mount ::Bench::UserAPI
  end
end

run Bench::API
