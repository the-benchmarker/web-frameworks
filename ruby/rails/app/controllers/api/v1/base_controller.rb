# Base controller for API v1
# All API v1 controllers inherit from this
#
# HTTP Standards Compliance:
# - Content-Type header is automatically set to 'application/json' in Rails API mode
# - RFC 7231: Proper status codes and response formatting
# - RFC 7807: Problem Details for error responses

module Api
  module V1
    class BaseController < ApplicationController
      # All API responses are JSON by default (handled by Rails API mode)

      # Ensure Date header is present (RFC 7231 Section 7.1.1.1)
      before_action :ensure_date_header

      private

      def ensure_date_header
        response.headers["Date"] ||= Time.now.httpdate
      end

      # Helper to render JSON with consistent structure
      # RFC 7231 Section 3.1.1.5: Content-Type header
      def render_json(data, status: :ok, **headers)
        headers.each { |k, v| response.headers[k] = v }
        render json: data, status: status
      end

      # Helper to render error JSON
      # Follows RFC 7807 (Problem Details) structure
      # RFC 7231: Appropriate status codes for errors
      def render_error(error, message, status, **headers)
        data = {
          error: error,
          message: message,
          status: status,
          timestamp: Time.current.utc.iso8601,
        }
        render_json(data, status: status, **headers)
      end
    end
  end
end
