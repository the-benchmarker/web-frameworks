Rails.application.configure do
  config.enable_reloading = true
  config.eager_load = false
  config.consider_all_requests_local = true
  config.server_timing = true
  config.public_file_server.headers = { 'Cache-Control' => 'no-store' }
  config.active_support.deprecation = :silence
  config.action_controller.raise_on_missing_callback_actions = true
end
