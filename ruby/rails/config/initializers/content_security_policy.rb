# Be sure to restart your server when you modify this file.

# Production-grade Content Security Policy
# For further information about the following directives, see https://content-security-policy.com/
Rails.application.configure do
  config.content_security_policy do |policy|
    # Default: Only allow from self and HTTPS
    policy.default_src :self, :https
    
    # Script sources
    policy.script_src :self, :https
    
    # Style sources
    policy.style_src :self, :https, :unsafe_inline
    
    # Image sources
    policy.img_src :self, :https, :data
    
    # Font sources
    policy.font_src :self, :https, :data
    
    # Object, embed, applet sources - none for security
    policy.object_src :none
    
    # Media sources
    policy.media_src :self, :https
    
    # Frame sources
    policy.frame_src :none
    
    # Form action targets
    policy.form_action :self, :https
    
    # Base URI restriction
    policy.base_uri :self
    
    # Frame ancestors - prevent clickjacking
    policy.frame_ancestors :none
    
    # Specify URI for violation reports (uncomment in production)
    # policy.report_uri "/csp-violation-report-endpoint"
    
    # Worker sources
    policy.worker_src :self, :https
    
    # Manifest sources
    policy.manifest_src :self
  end

  # Enable report-only mode in development for testing
  # In production, use enforce mode (default)
  if Rails.env.development?
    config.content_security_policy_report_only = true
  end
end
