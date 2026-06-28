require 'agoo'

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'
HOST = ENV.fetch('HOST', '0.0.0.0')
PORT = ENV.fetch('PORT', '3000').to_i

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

worker_count = ENV.fetch('WORKERS', `nproc`.to_i).to_i
worker_count = 1 if worker_count < 1

# Security headers configuration
SECURITY_HEADERS = {
  'X-Content-Type-Options' => 'nosniff',
  'X-Frame-Options' => 'DENY',
  'X-XSS-Protection' => '1; mode=block',
  'Content-Security-Policy' => "default-src 'self'",
  'Referrer-Policy' => 'strict-origin-when-cross-origin',
  'Cache-Control' => 'no-cache, no-store, must-revalidate'
}.freeze

# Startup message with configuration summary
if DEBUG_MODE
  puts "\n=== Agoo Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Workers: #{worker_count}"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "===============================================================\n\n"
else
  puts "\n=== Agoo Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "Workers: #{worker_count}"
  puts "===============================================================\n\n"
end

Agoo::Server.init(PORT, '.', thread_count: 0, worker_count:, poll_timeout: 0.1)

# Empty response.
class Empty
  def self.call(_req)
    [200, SECURITY_HEADERS.merge({'Content-Type' => 'text/plain'}), []]
  end

  def static?
    true
  end
end

# Reflects the id as the returned value.
class Reflect
  def self.call(req)
    [200, SECURITY_HEADERS.merge({'Content-Type' => 'text/plain'}), [req['PATH_INFO'][6..]]]
  end
end

# post response.
class Post
  def self.call(_req)
    [201, SECURITY_HEADERS.merge({'Content-Type' => 'text/plain'}), []]
  end
end

# Health check endpoint
class Health
  def self.call(_req)
    [200, SECURITY_HEADERS.merge({'Content-Type' => 'text/plain'}), ['OK']]
  end
end

# Error test endpoint
class ErrorTest
  def self.call(_req)
    if DEBUG_MODE
      [500, SECURITY_HEADERS.merge({'Content-Type' => 'text/plain'}), ['Internal Server Error']]
    else
      [500, SECURITY_HEADERS.merge({'Content-Type' => 'text/plain'}), []]
    end
  end
end

# Define routes
Agoo::Server.handle(:GET, '/', Empty)
Agoo::Server.handle(:GET, '/user/*', Reflect)
Agoo::Server.handle(:POST, '/user', Post)
Agoo::Server.handle(:GET, '/health', Health)
Agoo::Server.handle(:GET, '/error', ErrorTest)

Agoo::Server.start
