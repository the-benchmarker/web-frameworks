# frozen_string_literal: true

require 'cuba'

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'
HOST = ENV.fetch('HOST', '0.0.0.0')
PORT = ENV.fetch('PORT', '3000').to_i

# Security headers configuration - frozen for performance
SECURITY_HEADERS = {
  'X-Content-Type-Options' => 'nosniff',
  'X-Frame-Options' => 'DENY',
  'X-XSS-Protection' => '1; mode=block',
  'Content-Security-Policy' => "default-src 'self'",
  'Referrer-Policy' => 'strict-origin-when-cross-origin',
  'Cache-Control' => 'no-cache, no-store, must-revalidate'
}.freeze

# Custom logger for Cuba - disabled in production
Cuba.plugin Cuba::Logger if DEBUG_MODE

# Startup message with configuration summary
puts "\n=== Cuba Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Host: #{HOST}, Port: #{PORT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "===========================================================\n\n"

Cuba.define do
  # Apply security headers to all responses
  before do
    SECURITY_HEADERS.each { |key, value| res[key] = value }
    res['Content-Type'] = 'text/plain'
  end

  on get do
    on root do
      puts "[DEBUG] Root endpoint accessed" if DEBUG_MODE
      res.write ''
    end
    
    on 'user/:id' do |id|
      puts "[DEBUG] User endpoint accessed with ID: #{id}" if DEBUG_MODE
      res.write id
    end
    
    on 'health' do
      puts "[DEBUG] Health check endpoint accessed" if DEBUG_MODE
      res.write 'OK'
    end
    
    on 'error' do
      puts "[ERROR] Error endpoint accessed" if DEBUG_MODE
      res.status = 500
      res.write DEBUG_MODE ? 'Internal Server Error' : ''
    end
  end
  
  on post do
    on 'user' do
      puts "[DEBUG] Create user endpoint accessed" if DEBUG_MODE
      res.status = 201
      res.write ''
    end
  end
end
