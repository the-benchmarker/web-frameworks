require 'cuba'

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'
HOST = ENV.fetch('HOST', '0.0.0.0')
PORT = ENV.fetch('PORT', '3000').to_i

# Security headers configuration
SECURITY_HEADERS = {
  'X-Content-Type-Options' => 'nosniff',
  'X-Frame-Options' => 'DENY',
  'X-XSS-Protection' => '1; mode=block',
  'Content-Security-Policy' => "default-src 'self'",
  'Referrer-Policy' => 'strict-origin-when-cross-origin',
  'Cache-Control' => 'no-cache, no-store, must-revalidate'
}

# Custom logger for Cuba - disabled in production
if DEBUG_MODE
  Cuba.plugin Cuba::Logger
end

# Startup message with configuration summary
if DEBUG_MODE
  puts "\n=== Cuba Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "===========================================================\n\n"
else
  puts "\n=== Cuba Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "===========================================================\n\n"
end

Cuba.define do
  # Apply security headers to all responses
  before do
    SECURITY_HEADERS.each do |key, value|
      res[key] = value
    end
    res['Content-Type'] = 'text/plain'
  end

  on get do
    on root do
      if DEBUG_MODE
        puts "[DEBUG] Root endpoint accessed"
      end
      res.write ''
    end
    on 'user/:id' do |id|
      if DEBUG_MODE
        puts "[DEBUG] User endpoint accessed with ID: #{id}"
      end
      res.write id
    end
    on 'health' do
      if DEBUG_MODE
        puts "[DEBUG] Health check endpoint accessed"
      end
      res.write 'OK'
    end
    on 'error' do
      if DEBUG_MODE
        puts "[ERROR] Error endpoint accessed"
      end
      res.status = 500
      res.write DEBUG_MODE ? 'Internal Server Error' : ''
    end
  end
  on post do
    on 'user' do |_id|
      if DEBUG_MODE
        puts "[DEBUG] Create user endpoint accessed"
      end
      res.status = 201
      res.write ''
    end
  end
end
