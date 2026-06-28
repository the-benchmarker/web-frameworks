require "hanami"

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'

# Startup message with configuration summary
if DEBUG_MODE
  puts "\n=== Hanami Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "============================================================\n\n"
else
  puts "\n=== Hanami Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "============================================================\n\n"
end

module Benchmark
  class App < Hanami::App
    # Configure logger based on environment
    if DEBUG_MODE
      config.logger.level = :debug
      config.logger.stream = STDOUT
    else
      config.logger.stream = File::NULL
    end
  end
end
