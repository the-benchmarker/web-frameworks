require_relative "boot"

require "rails"

# Minimal framework requirements for API-only backend
require "active_model/railtie"
require "active_record/railtie"
require "action_controller/railtie"

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(*Rails.groups)

module Benchmark
  class Application < Rails::Application
    # Initialize configuration defaults for originally generated Rails version.
    config.load_defaults 8.0

    # API-only mode - no frontend, no templates, no assets
    config.api_only = true

    # Don't load middleware we don't need
    config.middleware.delete ActionDispatch::Cookies
    config.middleware.delete ActionDispatch::Session::CookieStore
    config.middleware.delete ActionDispatch::Flash
    config.middleware.delete Rack::MethodOverride

    # Keep Head, ConditionalGet, and ETag for HTTP standards compliance
    # Rack::ConditionalGet and Rack::ETag are required for RFC 7232 (Conditional Requests)
    # ActionDispatch::Head is required for RFC 7231 (Semantics and Content)

    # Add our custom CORS middleware (must come before ActionDispatch::Head)
    require_relative "../app/middleware/cors_middleware"
    config.middleware.use CorsMiddleware

    # Auto-loading configuration
    config.autoload_lib(ignore: %w[assets tasks])

    # Configuration for security headers (controlled via initializers)
    # All headers are configured in config/initializers/security.rb

    # Time zone
    config.time_zone = "UTC"
  end
end
