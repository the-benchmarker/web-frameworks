# Security configuration for Rails API
# All security headers are configured here, not hardcoded in controllers
#
# HTTP Standards Compliance Notes:
# - RFC 7231 Section 7.1.1.1: Date header is REQUIRED for all responses
# - RFC 7232: ETag and Last-Modified headers for conditional requests
# - RFC 6797: HSTS header for HTTPS enforcement
# - RFC 7234: Cache-Control header for caching behavior

Rails.application.configure do
  # ==========================================================================
  # DEFAULT HEADERS - Applied to all responses
  # ==========================================================================

  # Configure default headers that will be applied to every response
  # Standard headers only - no custom X- headers (per RFC 6648 and proxy compatibility)
  config.action_dispatch.default_headers = {
    "Date" => -> { Time.now.httpdate },
  }

  # ==========================================================================
  # SECURITY HEADERS
  # ==========================================================================
  # Note: X-Frame-Options, X-Content-Type-Options, and X-XSS-Protection headers
  # have been removed for reverse proxy compatibility as requested.
  # These should be configured at the web server / reverse proxy level instead.
  #
  # Content-Security-Policy: Modern replacement for X-Frame-Options and X-XSS-Protection
  # Configured in content_security_policy.rb initializer

  # Referrer-Policy: Control referrer information (RFC 6962)
  config.action_dispatch.referrer_policy = "strict-origin-when-cross-origin"

  # Permissions-Policy: Control browser features (W3C standard)
  config.action_dispatch.permissions_policy =
    "geolocation=(), microphone=(), camera=(), payment=()"

  # ==========================================================================
  # HSTS (HTTP Strict Transport Security) - RFC 6797
  # ==========================================================================
  # Only enable in production with HTTPS
  if Rails.env.production?
    config.force_ssl = true
    config.ssl_options = {
      redirect: { exclude: ->(request) { request.path == "/health" } },
      hsts: {
        expires: 1.year,
        subdomains: true,
        preload: true,
        include_subdomains: true,
      },
    }
  end

  # ==========================================================================
  # CACHE CONTROL - RFC 7234
  # ==========================================================================
  # Default cache control for API responses
  # Can be overridden per-controller or per-action
  # no-cache: Forces caches to submit the request to the origin server
  # no-store: Prevents caching of sensitive data
  # must-revalidate: Requires revalidation with origin server
  config.action_dispatch.cache_control = {
    max_age: 0,
    public: false,
    must_revalidate: true,
    no_cache: true,
    no_store: false,
  }
end

# ==========================================================================
# REQUEST LOGGING FILTER
# ==========================================================================
# Filter sensitive parameters from logs
# RFC 7230 Section 3.2.6: Sensitive header field names should be redacted
Rails.application.config.filter_parameters += [
  :password, :secret, :token, :_key, :crypt, :salt, :certificate, :otp, :ssn,
  :credit_card, :api_key, :access_token, :auth_token, :refresh_token,
]
