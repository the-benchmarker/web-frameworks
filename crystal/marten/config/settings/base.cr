Marten.configure do |config|
  config.debug = false
  config.host = "0.0.0.0"
  config.port = ENV["PORT"]? ? ENV["PORT"].to_i : 3000
  config.port_reuse = true
  config.allowed_hosts = ["*"]
  config.log_level = Log::Severity::None
end
