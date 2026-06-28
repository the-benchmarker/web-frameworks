#!/usr/bin/env crystal
# Lucky Framework Production Configuration
# Security and performance optimized for production environments

# Logging: Completely disable logging for production performance
# Use external logging infrastructure (ELK, Datadog, etc.) in production
Log.setup :none

# Session configuration - security hardened
Lucky::Session.configure do |settings|
  settings.key = ENV["SESSION_KEY"]? || "_benchmark_session"
  settings.secret = ENV["SESSION_SECRET"]? || "u4PWnhZfOFXdTOtoiSBF+6jn0zHbYS6/yumo3WXYNSw="
  settings.domain = nil  # Default: current domain
  settings.path = "/"
  settings.expires = 0  # Session cookies don't expire when browser closes
  settings.secure = true  # Only send over HTTPS
  settings.http_only = true  # Prevent JavaScript access
  settings.same_site = :lax  # CSRF protection
end

# Error handling - production optimized
Lucky::ErrorHandler.configure do |settings|
  settings.show_debug_output = false  # Never show debug info in production
  settings.show_stack_trace = false  # Never show stack traces in production
  settings.log_errors = false  # Don't log errors (external logging handles this)
end

# Server configuration - production optimized
Lucky::Server.configure do |settings|
  settings.secret_key_base = ENV["SECRET_KEY_BASE"]? || "u4PWnhZfOFXdTOtoiSBF+6jn0zHbYS6/yumo3WXYNSw="
  settings.host = ENV["HOST"]? || "0.0.0.0"
  settings.port = (ENV["PORT"]? || 3000).to_i
  settings.port_reuse = true  # Enable port reuse for clustering
  settings.allowed_hosts = ["*"]  # Allow all hosts (adjust for security)
end
