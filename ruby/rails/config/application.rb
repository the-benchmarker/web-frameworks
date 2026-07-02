# frozen_string_literal: true

require_relative "boot"

require "rails"
# Pick the frameworks you want:
require "active_model/railtie"
require "active_job/railtie"
# require "active_record/railtie"
# require "active_storage/engine"
require "action_controller/railtie"
require "action_mailer/railtie"
# require "action_mailbox/engine"
# require "action_text/engine"
# require "action_view/railtie"
# require "action_cable/engine"
# require "rails/test_unit/railtie"

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(*Rails.groups)

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'

# Startup message with configuration summary
puts "\n=== Rails Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "=========================================================\n\n"

module Benchmark
  class Application < Rails::Application
    # Initialize configuration defaults for originally generated Rails version.
    config.load_defaults 8.0

    # Optimize for benchmarking - disable unnecessary features
    config.autoload_lib(ignore: %w[assets tasks])
    
    # Remove middleware that is not needed for an API-only Rails app
    config.middleware.delete ActionDispatch::Cookies
    config.middleware.delete ActionDispatch::Session::CookieStore
    config.middleware.delete ActionDispatch::RemoteIp
    config.middleware.delete ActionDispatch::RequestId
    config.middleware.delete ActionDispatch::ShowExceptions
    
    # API only configuration
    config.api_only = true
    config.action_controller.allow_forgery_protection = false
    
    # Performance optimizations for benchmarking
    config.eager_load = true
    config.cache_classes = true
    config.consider_all_requests_local = true
    config.public_file_server.enabled = false
  end
end
