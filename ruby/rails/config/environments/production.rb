Rails.application.configure do
  config.enable_reloading = false
  config.eager_load = true
  config.consider_all_requests_local = false
  config.public_file_server.enabled = true
  config.public_file_server.headers = { 'Cache-Control' => 'no-store' }
  config.i18n.fallbacks = true
  config.active_support.report_deprecations = false
end
