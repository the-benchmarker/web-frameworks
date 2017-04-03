# frozen-string-literal: true

require 'set'

class Roda
  module RodaPlugins
    # The request_headers plugin provides access to headers sent in the
    # request in a more natural way than directly accessing the env hash.
    #
    # In practise this means you don't need to uppercase, convert dashes
    # to underscores, or add a HTTP_ prefix.
    #
    # For example, to access a header called X-My-Header you
    # would previously need to do:
    #
    #   r.env['HTTP_X_MY_HEADER']
    #
    # But with this plugin you can now say:
    #
    #   r.headers['X-My-Header']
    #
    # The name is actually case-insensitive so x-my-header will work as well.
    #
    #
    # Example:
    #
    #   plugin :request_headers
    #
    module RequestHeaders
      module RequestMethods
        # Provide access to the request headers while normalising indexes.
        def headers
          @request_headers ||= Headers.new(@env)
        end
      end

      class Headers
        # Set of environment variable names that don't need HTTP_ prepended to them.
        CGI_VARIABLES = Set.new(%w'
          AUTH_TYPE
          CONTENT_LENGTH
          CONTENT_TYPE
          GATEWAY_INTERFACE
          HTTPS
          PATH_INFO
          PATH_TRANSLATED
          QUERY_STRING
          REMOTE_ADDR
          REMOTE_HOST
          REMOTE_IDENT
          REMOTE_USER
          REQUEST_METHOD
          SCRIPT_NAME
          SERVER_NAME
          SERVER_PORT
          SERVER_PROTOCOL
          SERVER_SOFTWARE
        ').freeze

        def initialize(env)
          @env = env
        end

        # Returns the value for the given key mapped to @env
        def [](key)
          @env[env_name(key)]
        end

        private

        # Convert a HTTP header name into an environment variable name
        def env_name(key)
          key = key.to_s.upcase
          key.tr!('-', '_')
          key = 'HTTP_' + key unless CGI_VARIABLES.include?(key)
          key
        end
      end
    end

    register_plugin(:request_headers, RequestHeaders)
  end
end
