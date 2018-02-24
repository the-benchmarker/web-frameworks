def app
  Rack::Builder.new{
    run App
  }.to_app
end
