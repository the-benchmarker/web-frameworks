# frozen_string_literal: true

require "hanami"

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'

# Startup message with configuration summary
puts "\n=== Hanami Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "============================================================\n\n"

module Benchmark
  class App < Hanami::App
    # Configure logger based on environment
    config.logger.level = DEBUG_MODE ? :debug : :warn
    config.logger.stream = DEBUG_MODE ? STDOUT : File::NULL
  end
end
