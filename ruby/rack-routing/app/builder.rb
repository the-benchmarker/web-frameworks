# frozen_string_literal: true

def app
  Rack::Builder.new do
    run App
  end.to_app
end
