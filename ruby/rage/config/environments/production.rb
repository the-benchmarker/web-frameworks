require 'logger'

Rage.configure do
  config.server.port = ENV.fetch('PORT', '3000').to_i
  config.host = ENV.fetch('HOST', '0.0.0.0')
  
  # Configure logging based on environment
  if DEBUG_MODE
    config.logger = Logger.new(STDOUT)
    config.logger.level = Logger::DEBUG
  else
    config.logger = nil
  end
end
