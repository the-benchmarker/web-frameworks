# frozen_string_literal: true

require 'camping'

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

# Security headers middleware for Camping
module SecurityHeaders
  def self.included(controller)
    controller.before do
      @headers.merge!(SECURITY_HEADERS)
    end
  end
end

# Custom logger for Camping - disabled in production
module CampingLogger
  def self.included(controller)
    controller.before do
      puts "[DEBUG] #{@headers['REQUEST_METHOD']} - #{@env['PATH_INFO']}" if DEBUG_MODE
    end
  end
end

# Startup message with configuration summary
puts "\n=== Camping Framework Benchmark Server (#{DEBUG_MODE ? 'Development' : 'Production'} Mode) ==="
puts "Environment: #{ENVIRONMENT}"
puts "Host: #{HOST}, Port: #{PORT}"
puts "Debug: #{DEBUG_MODE}, Security headers: Enabled"
puts "Logging: #{DEBUG_MODE ? 'Enabled' : 'Disabled'}"
puts "Endpoints: /, /user/:id, /user, /health, /error"
puts "===============================================================\n\n"

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
        DEBUG_MODE ? 'Internal Server Error' : ''
      end
    end
  end
end
