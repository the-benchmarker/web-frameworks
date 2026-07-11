require "active_support/core_ext/integer/time"

Rails.application.configure do
  # Code is not reloaded between requests
  config.enable_reloading = false

  # Eager load code on boot
  config.eager_load = true

  # Full error reports are disabled and caching is turned on
  config.consider_all_requests_local = false
  config.action_controller.perform_caching = true

  # Enable static file serving from the `/public` folder (for health checks)
  config.public_file_server.enabled = true

  # Logger configuration
  config.logger = ActiveSupport::Logger.new($stdout).tap do |logger|
    logger.formatter = Logger::Formatter.new
    logger.level = ENV.fetch("RAILS_LOG_LEVEL", "info")
  end.then { |logger| ActiveSupport::TaggedLogging.new(logger) }

  config.log_tags = [:request_id]

  # i18n fallback
  config.i18n.fallbacks = true

  # Don't log deprecations
  config.active_support.report_deprecations = false
end
