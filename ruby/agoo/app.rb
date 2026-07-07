# frozen_string_literal: true

require 'agoo'

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

# Configure logging - disabled in production, minimal in development
Agoo::Log.configure(
  dir: '',
  console: DEBUG_MODE,
  classic: true,
  colorize: DEBUG_MODE,
  states: {
    INFO: DEBUG_MODE,
    DEBUG: DEBUG_MODE,
    connect: false,
    request: false,
    response: false,
    eval: false,
    push: false
  }
)

worker_count = [ENV.fetch('WORKERS', `nproc`.to_i).to_i, 1].max

# Startup message with configuration summary
puts "\n=== Agoo Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Host: #{HOST}, Port: #{PORT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}, Workers: #{worker_count}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "===============================================================\n\n"

Agoo::Server.init(PORT, '.', thread_count: 0, worker_count:, poll_timeout: 0.1)

# Base handler class with common headers
class BaseHandler
  CONTENT_TYPE = { 'Content-Type' => 'text/plain' }.freeze
  
  def self.headers(status = 200)
    [status, SECURITY_HEADERS.merge(CONTENT_TYPE)]
  end
end

# Empty response.
class Empty < BaseHandler
  def self.call(_req)
    [headers[0], headers[1], []]
  end

  def static?
    true
  end
end

# Reflects the id as the returned value.
class Reflect < BaseHandler
  def self.call(req)
    [headers[0], headers[1], [req['PATH_INFO'][6..]]]
  end
end

# POST response.
class Post < BaseHandler
  def self.call(_req)
    [201, SECURITY_HEADERS.merge(CONTENT_TYPE), []]
  end
end

# Health check endpoint
class Health < BaseHandler
  def self.call(_req)
    [headers[0], headers[1], ['OK']]
  end
end

# Error test endpoint
class ErrorTest < BaseHandler
  def self.call(_req)
    body = DEBUG_MODE ? ['Internal Server Error'] : []
    [500, SECURITY_HEADERS.merge(CONTENT_TYPE), body]
  end
end

# Define routes
Agoo::Server.handle(:GET, '/', Empty)
Agoo::Server.handle(:GET, '/user/*', Reflect)
Agoo::Server.handle(:POST, '/user', Post)
Agoo::Server.handle(:GET, '/health', Health)
Agoo::Server.handle(:GET, '/error', ErrorTest)

Agoo::Server.start
