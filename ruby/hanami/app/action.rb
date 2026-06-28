require "hanami/action"

# Security headers configuration
SECURITY_HEADERS = {
  'X-Content-Type-Options' => 'nosniff',
  'X-Frame-Options' => 'DENY',
  'X-XSS-Protection' => '1; mode=block',
  'Content-Security-Policy' => "default-src 'self'",
  'Referrer-Policy' => 'strict-origin-when-cross-origin',
  'Cache-Control' => 'no-cache, no-store, must-revalidate'
}.freeze

module Benchmark
  class Action < Hanami::Action
    # Apply security headers to all responses
    before :apply_security_headers
    
    private
    
    def apply_security_headers
      SECURITY_HEADERS.each do |key, value|
        response.headers[key] = value
      end
    end
  end
end
