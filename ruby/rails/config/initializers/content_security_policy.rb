# Be sure to restart your server when you modify this file.

# Define an application-wide content security policy
# For further information about the following directives, see https://content-security-policy.com/
Rails.application.configure do
  config.content_security_policy do |policy|
    policy.default_src :self, :https
    policy.font_src    :self, :https, :data
    policy.img_src     :self, :https, :data
    policy.object_src  :none
    policy.script_src  :self, :https
    policy.style_src   :self, :https
    # Specify URI for violation reports
    # policy.report_uri "/csp-violation-report-endpoint"
  end

  # Generate nonces for meta tags that allow script execution
  # config.content_security_policy_nonce_generator = ->(request) { request.session.id.to_s }

  # Enable the report-only mode. The policy will be sent with Content-Security-Policy-Report-Only header
  # instead of Content-Security-Policy header, and the browser will not block loading of resources.
  # This is useful for testing the policy without breaking the application.
  # config.content_security_policy_report_only = true
end
