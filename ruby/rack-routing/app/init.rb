# frozen_string_literal: true

require("rubygems") unless defined? ::Gem

# Gems
require("rack")
require("rack/routing")

# App
require_relative("app")
require_relative("builder")
require_relative("request")
require_relative("route_handler")

# Ruby modules
require("json")

# I18n.enforce_available_locales = true

ROUTES_FILE = "config/routes.txt"
Router = Rack::Routing::Router
ROUTES = Router.load_routes
