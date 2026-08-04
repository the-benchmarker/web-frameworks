# API v1 System Controller
# Category 6: API & Integration
#
# HTTP Standards Compliance:
# - RFC 7231: HTTP Semantics and Content
# - RFC 7234: HTTP/1.1 Caching
# - RFC 7232: Conditional Requests

module Api
  module V1
    class SystemController < BaseController
      # GET /api/v1/json - JSON API information
      # RFC 7234: Cache-Control headers for non-cacheable responses
      def info
        data = {
          status: "ok",
          message: "JSON API response",
          timestamp: Time.current.utc.iso8601,
          framework: "Rails",
          version: Rails.version,
          api_version: "1.0.0",
        }
        # RFC 7234 Section 5.2.2: no-cache, no-store, must-revalidate
        render_json(data, cache_control: "no-cache, no-store, must-revalidate")
      end

      # GET /api/v1/external - External API simulation
      # RFC 7234: Cacheable response with max-age
      def external
        data = {
          id: 1,
          title: "External resource",
          source: "mock_external_api",
          timestamp: Time.current.utc.iso8601,
        }
        # RFC 7234 Section 5.2.2: public cache with max-age
        render_json(data, cache_control: "max-age=300, public")
      end

      # GET /api/v1/cached - Cached response with proper cache headers
      # RFC 7234: HTTP/1.1 Caching
      # RFC 7232: ETag for conditional requests
      def cached
        cache_key = "api:cached_response"
        cached_data = Rails.cache.read(cache_key)

        # RFC 7232: Generate ETag for the response
        if cached_data.nil?
          data = {
            data: "Cached response",
            cached_at: Time.current.utc.iso8601,
            cache_status: "fresh",
          }
          Rails.cache.write(cache_key, data, expires_in: 1.hour)
        else
          data = cached_data.merge(cache_status: "served_from_cache")
        end

        # RFC 7232 Section 2.3: ETag header
        etag = Digest::MD5.hexdigest(data.to_json)

        # RFC 7234 Section 5.2.2: Cache-Control headers
        # max-age: Time in seconds the response is considered fresh
        # public: Response can be cached by any cache
        response.headers["ETag"] = etag
        render_json(data, cache_control: "max-age=3600, public")
      end
    end
  end
end
