#!/usr/bin/env crystal
# Marten Framework Production Configuration
# Security and performance optimized for production environments

Marten.configure do |config|
  # Security: Disable debug mode
  config.debug = false
  
  # Host and port configuration
  config.host = ENV["HOST"]? || "0.0.0.0"
  config.port = ENV["PORT"]? ? ENV["PORT"].to_i : 3000
  
  # Port reuse for clustering
  config.port_reuse = true
  
  # Security: Allow all hosts (adjust for production security)
  config.allowed_hosts = ["*"]
  
  # Logging: Completely disable logging for production performance
  config.log_level = Log::Severity::None
  
  # Security headers
  config.secure_headers = true
  
  # Session configuration (if using sessions)
  # config.session_key = ENV["SESSION_KEY"]? || "_marten_session"
  # config.session_secret = ENV["SESSION_SECRET"]
  # config.session_secure = true
  # config.session_http_only = true
  # config.session_same_site = :lax
end
