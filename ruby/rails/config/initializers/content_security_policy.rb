# Content Security Policy for API-only Rails application
# Since this is an API backend with no frontend, CSP is simplified

Rails.application.configure do
  # For API-only applications, we mainly need to prevent clickjacking
  # and other basic protections. Full CSP is more relevant for frontend apps.

  # CSP frame-ancestors replaces X-Frame-Options (which was removed for proxy compatibility)
  # frame-ancestors :none is equivalent to X-Frame-Options: DENY
  config.content_security_policy do |policy|
    policy.frame_ancestors :none
  end
end
