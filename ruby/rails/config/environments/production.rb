Rails.application.configure do
  config.enable_reloading = false
  config.eager_load = true
  config.consider_all_requests_local = false
  config.public_file_server.enabled = false
  config.active_support.report_deprecations = false

  # The benchmark runs over plain HTTP, as required by SCENARIO.md.
  config.force_ssl = false
  config.assume_ssl = false
end
