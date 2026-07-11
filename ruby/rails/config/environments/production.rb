require "active_support/core_ext/integer/time"

Rails.application.configure do
  # Code is not reloaded between requests
  config.enable_reloading = false

  # Eager load code on boot
  config.eager_load = true

  # Full error reports are disabled and caching is turned on
  config.consider_all_requests_local = false
  config.action_controller.perform_caching = true

  # Enable static file serving from the `/public` folder
  config.public_file_server.enabled = true

  # Logger configuration
  config.logger = ActiveSupport::Logger.new($stdout).tap do |logger|
    logger.formatter = Logger::Formatter.new
    logger.level = ENV.fetch("RAILS_LOG_LEVEL", "info")
  end.then { |logger| ActiveSupport::TaggedLogging.new(logger) }

  config.log_tags = [:request_id]

  # Action Mailer configuration
  config.action_mailer.perform_caching = false

  # i18n fallback
  config.i18n.fallbacks = true

  # Don't log deprecations
  config.active_support.report_deprecations = false

  # Security: Force SSL in production
  config.force_ssl = true

  # Security: SSL options
  config.ssl_options = {
    redirect: { exclude: ->(request) { request.path == '/health' } },
    hsts: { expires: 1.year, subdomains: true, preload: true }
  }

  # Security: Session configuration
  config.session_store :cookie_store, {
    key: '_benchmark_session',
    secure: true,
    httponly: true,
    same_site: :lax
  }

  # Security: Cookie configuration
  config.action_dispatch.cookies = {
    secure: true,
    httponly: true,
    same_site: :lax
  }
end
