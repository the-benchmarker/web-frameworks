# Application dependencies
require "action-controller"
require "active-model"

# Application code
require "./controllers/application"
require "./controllers/*"
require "./models/*"

# Server required after application controllers
require "action-controller/server"

# Configure session cookies
# NOTE:: Change these from defaults
ActionController::Session.configure do
  settings.key = ENV["COOKIE_SESSION_KEY"]? || "_spider_gazelle_"
  settings.secret = ENV["COOKIE_SESSION_SECRET"]? || "4f74c0b358d5bab4000dd3c75465dc2c"
end

APP_NAME = "Spider-Gazelle"
VERSION  = "1.0.0"
