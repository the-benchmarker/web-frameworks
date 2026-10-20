Rails.application.configure do
  config.enable_reloading = false
  config.eager_load = ENV['CI'].present?
  config.consider_all_requests_local = true
  config.public_file_server.enabled = false
  # Unexpected exceptions reach the tests; Rails handles known request errors.
  config.action_dispatch.show_exceptions = :rescuable
  config.active_support.deprecation = :silence
  config.action_controller.raise_on_missing_callback_actions = true
end
