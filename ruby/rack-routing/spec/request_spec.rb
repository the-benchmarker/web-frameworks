require 'spec_helper'

describe 'Requests' do
  specify 'GET /' do
    get '/'

    expect( last_response.status ).to eq  200
    expect( last_response.body   ).to eq ''
  end

  specify 'GET /user/123' do
    get '/user/123'

    expect( last_response.status ).to eq  200
    expect( last_response.body   ).to eq '123'
  end

  specify 'POST /user' do
    post '/user'

    expect( last_response.status ).to eq  200
    expect( last_response.body   ).to eq ''
  end
end