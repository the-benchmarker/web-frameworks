require "amber"

require "../src/controllers/application_controller"
require "../src/controllers/**"
require "./routes"

# About Application.cr File
#
# This is Amber application main entry point. This file is responsible for loading
# initializers, classes, and all application related code in order to have
# Amber::Server boot up.
#
# > We recommend to not modify the order of the require since the order will
# affect the behavior of the application.
#
# With `Amber::Server.configure` block you can redefine the Server configuration
# settings and use ENVIRONMENT variables and/or values evaluated at runtime.
#
# > Important! Yaml configurations are first class citizen and are loaded first before
# this file, we recommend to use yaml configurations before changing any settings here.
# Any uncommented setting here will override the YAML with the value set here.

Amber::Server.configure do |settings|
  # Production-grade Amber configuration
  # Security and performance optimized for production environments
  
  # Application name
  settings.name = "Amber"
  
  # Host configuration - bind to all interfaces for containerized deployments
  settings.host = ENV["HOST"]? || "0.0.0.0"
  
  # Port configuration - respect PORT environment variable (common in cloud platforms)
  settings.port = ENV["PORT"].to_i if ENV["PORT"]?
  
  # Port reuse for clustering - enables multiple workers on same port
  settings.port_reuse = true
  
  # Process count - optimize for production (leave at least 1 core for system)
  # Uncomment and adjust based on your server's CPU count
  # settings.process_count = [System.cpu_count.to_i32 - 1, 1].max
  
  # Logging configuration - production optimized
  settings.logging.colorize = false
  settings.logging.severity = ENV["LOG_LEVEL"]? || "error"
  settings.logging.filter = %w(password confirm_password token secret authorization)
  settings.logging.context = %w(request session)
  
  # Session configuration - security hardened
  settings.session = {
    "key" => ENV["SESSION_KEY"]? || "amber.session",
    "store" => ENV["SESSION_STORE"]? || "encrypted_cookie",
    "expires" => 0,
    "secure" => true,
    "httponly" => true,
    "same_site" => "lax"
  }
  
  # Redis URL - for session storage or caching
  settings.redis_url = ENV["REDIS_URL"] if ENV["REDIS_URL"]?
  
  # Database URL - production database connection
  settings.database_url = ENV["DATABASE_URL"] if ENV["DATABASE_URL"]?
  
  # SSL/TLS configuration (uncomment when using HTTPS)
  # settings.ssl_key_file = ENV["SSL_KEY_FILE"] if ENV["SSL_KEY_FILE"]?
  # settings.ssl_cert_file = ENV["SSL_CERT_FILE"] if ENV["SSL_CERT_FILE"]?
end
