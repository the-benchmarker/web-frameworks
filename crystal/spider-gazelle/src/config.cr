#!/usr/bin/env crystal
# Spider-Gazelle Application Configuration
# Security and performance optimized for production environments

# Application dependencies
require "action-controller"
require "log"

# Application code
require "./controllers/application"
require "./controllers/*"

# Server required after application controllers
require "action-controller/server"

# Application configuration
APP_NAME = "Spider-Gazelle"
VERSION = "1.0.0"

# Logging: Completely disable logging for production performance
# Use external logging infrastructure (ELK, Datadog, etc.) in production
Log.setup(:none)

# Session configuration - security hardened
# NOTE: Change these from defaults for production
ActionController::Session.configure do |settings|
  settings.key = ENV["COOKIE_SESSION_KEY"]? || "_spider_gazelle_"
  settings.secret = ENV["COOKIE_SESSION_SECRET"]? || "4f74c0b358d5bab4000dd3c75465dc2c"
  settings.domain = nil  # Default: current domain
  settings.path = "/"
  settings.secure = true  # Only send over HTTPS
  settings.http_only = true  # Prevent JavaScript access
  settings.same_site = :lax  # CSRF protection
  settings.expires = 0  # Session cookies don't expire when browser closes
end

# Security: Disable debug mode
ActionController::Base.debugging = false