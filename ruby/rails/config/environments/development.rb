require "active_support/core_ext/integer/time"

Rails.application.configure do
  config.enable_reloading = true
  config.eager_load = false
  config.consider_all_requests_local = true
  config.action_controller.perform_caching = false
  config.public_file_server.enabled = true
  config.logger = ActiveSupport::Logger.new($stdout).tap { |logger| logger.formatter = Logger::Formatter.new }.then { |logger| ActiveSupport::TaggedLogging.new(logger) }
  config.log_level = :debug
  config.action_mailer.perform_caching = false
  config.i18n.fallbacks = true
  config.active_support.report_deprecations = true
end
