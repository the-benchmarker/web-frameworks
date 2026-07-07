require 'logger'

Rage.configure do
  config.server.port = ENV.fetch('PORT', '3000').to_i
  config.host = ENV.fetch('HOST', '0.0.0.0')
  
  # Enable logging in development
  config.logger = Logger.new(STDOUT)
  config.logger.level = Logger::DEBUG
  config.logger.formatter = proc { |severity, datetime, progname, msg|
    "[#{datetime}] #{severity} - #{msg}\n"
  }
end