# API v1 Security Controller
# Category 4: Security
#
# HTTP Standards Compliance:
# - RFC 7235: HTTP Authentication Framework
# - RFC 6750: Bearer Token Usage (OAuth 2.0)
# - RFC 7231: Status codes (401 Unauthorized)

module Api
  module V1
    class SecurityController < BaseController
      # GET /api/v1/secure - Bearer token authentication
      # RFC 6750 Section 2.1: Bearer Token in Authorization header
      # RFC 7235: WWW-Authenticate header for 401 responses
      def secure
        auth_header = request.headers["Authorization"]

        if auth_header.nil?
          return unauthorized("No authorization token provided", :missing_token)
        end

        unless auth_header.start_with?("Bearer ")
          return unauthorized("Invalid token type. Expected Bearer token", :invalid_token)
        end

        token = auth_header.split(" ", 2).last

        if token.blank?
          return unauthorized("Empty authorization token", :invalid_token)
        end

        # Mask token for security (RFC 6750 Section 5.3: Don't expose full token)
        masked_token = "#{token[0..7]}...#{token[-4..-1]}"

        data = {
          authenticated: true,
          token: masked_token,
          message: "Access granted",
          timestamp: Time.current.utc.iso8601,
        }

        # RFC 7235 Section 4.1: WWW-Authenticate header
        response.headers["WWW-Authenticate"] = 'Bearer realm="api"'
        render_json(data)
      end

      # GET /api/v1/protected - Protected resource
      # RFC 7235: Protected resource requires authentication
      def protected
        auth_header = request.headers["Authorization"]

        if auth_header.nil? || !auth_header.start_with?("Bearer ")
          # RFC 7235 Section 3: Authentication required
          response.headers["WWW-Authenticate"] = 'Bearer realm="api", error="invalid_token", error_description="Missing or invalid Bearer token"'
          return head :unauthorized
        end

        token = auth_header.split(" ", 2).last

        if token.blank?
          # RFC 7235 Section 3: Authentication required
          response.headers["WWW-Authenticate"] = 'Bearer realm="api", error="invalid_token", error_description="Empty token"'
          return head :unauthorized
        end

        data = {
          resource: "Protected data",
          access: "granted",
          protected: true,
          timestamp: Time.current.utc.iso8601,
        }

        render_json(data)
      end

      private

      # RFC 7235 Section 3: Authentication required
      # RFC 6750 Section 3: Error codes for Bearer tokens
      def unauthorized(description, error_code = :invalid_token)
        error_name = case error_code
          when :missing_token then "missing_token"
          when :invalid_token then "invalid_token"
          when :expired_token then "expired_token"
          else "invalid_token"
          end

        # RFC 7235 Section 4.1: WWW-Authenticate header format
        # RFC 6750 Section 3: Bearer token error codes
        response.headers["WWW-Authenticate"] = "Bearer realm=\"api\", error=\"#{error_name}\", error_description=\"#{description}\""
        render_error("Unauthorized", description, :unauthorized)
      end
    end
  end
end
