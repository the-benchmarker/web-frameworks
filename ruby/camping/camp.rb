require 'camping'

# Configuration - Environment-based settings for production vs development
DEBUG_MODE = ENV.fetch('DEBUG', 'false') == 'true'
ENVIRONMENT = DEBUG_MODE ? 'development' : 'production'
HOST = ENV.fetch('HOST', '0.0.0.0')
PORT = ENV.fetch('PORT', '3000').to_i

# Security headers middleware for Camping
module SecurityHeaders
  def self.included(controller)
    controller.before do
      @headers.merge!(
        'X-Content-Type-Options' => 'nosniff',
        'X-Frame-Options' => 'DENY',
        'X-XSS-Protection' => '1; mode=block',
        'Content-Security-Policy' => "default-src 'self'",
        'Referrer-Policy' => 'strict-origin-when-cross-origin',
        'Cache-Control' => 'no-cache, no-store, must-revalidate'
      )
    end
  end
end

# Custom logger for Camping - disabled in production
module CampingLogger
  def self.included(controller)
    controller.before do
      if DEBUG_MODE
        puts "[#{Time.now}] #{@headers['REQUEST_METHOD']} - #{@env['PATH_INFO']}"
      end
    end
  end
end

# Startup message with configuration summary
if DEBUG_MODE
  puts "\n=== Camping Framework Benchmark Server (Development Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Enabled (debug level)"
  puts "Endpoints: /, /user/:id, /user, /health, /error"
  puts "===============================================================\n\n"
else
  puts "\n=== Camping Framework Benchmark Server (Production Mode) ==="
  puts "Environment: #{ENVIRONMENT}"
  puts "Host: #{HOST}"
  puts "Port: #{PORT}"
  puts "Debug: #{DEBUG_MODE}"
  puts "Security headers: Enabled"
  puts "Logging: Disabled (production mode)"
  puts "===============================================================\n\n"
end

Camping.goes :App

module App
  module Controllers
    class Index < R '/'
      include SecurityHeaders
      include CampingLogger
      
      def get
        @headers['Content-Type'] = 'text/plain'
        ''
      end
    end

    class User < R '/user/(\d+)'
      include SecurityHeaders
      include CampingLogger
      
      def get(id)
        @headers['Content-Type'] = 'text/plain'
        id
      end
    end

    class Creator < R '/user'
      include SecurityHeaders
      include CampingLogger
      
      def post
        @headers['Content-Type'] = 'text/plain'
        @headers['Status'] = 201
        ''
      end
    end

    class Health < R '/health'
      include SecurityHeaders
      
      def get
        @headers['Content-Type'] = 'text/plain'
        'OK'
      end
    end

    class ErrorTest < R '/error'
      include SecurityHeaders
      
      def get
        @headers['Content-Type'] = 'text/plain'
        @headers['Status'] = 500
        if DEBUG_MODE
          'Internal Server Error'
        else
          ''
        end
      end
    end
  end
end
