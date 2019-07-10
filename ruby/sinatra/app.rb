# frozen_string_literal: true

require 'sinatra'

get '/' do
  # do nothing
end

get %r{/user/([\d]+)} do |id|
  body id.to_s
end

post '/user' do
  # do nothing
end
