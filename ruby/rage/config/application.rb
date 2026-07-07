# frozen_string_literal: true

require "bundler/setup"
require "rage"
Bundler.require(*Rage.groups)

require "rage/all"

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'
HOST = ENV.fetch('HOST', '0.0.0.0')
PORT = ENV.fetch('PORT', '3000').to_i

# Startup message with configuration summary
puts "\n=== Rage Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Host: #{HOST}, Port: #{PORT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "============================================================\n\n"

Rage.configure do
  # use this to add settings that are constant across all environments
  config.host = HOST
  config.server.port = PORT
end

require "rage/setup"
