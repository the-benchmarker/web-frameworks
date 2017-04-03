# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The heartbeat handles heartbeat/status requests.  If a request for
    # the heartbeat path comes in, a 200 response with a
    # text/plain Content-Type and a body of "OK" will be returned.
    # The default heartbeat path is "/heartbeat", so to use that:
    #
    #   plugin :heartbeat
    #
    # You can also specify a custom heartbeat path:
    #
    #   plugin :heartbeat, :path=>'/status'
    module Heartbeat
      OPTS = {}.freeze
      PATH_INFO = 'PATH_INFO'.freeze
      HEARTBEAT_RESPONSE = [200, {'Content-Type'=>'text/plain'}.freeze, ['OK'.freeze].freeze].freeze

      # Set the heartbeat path to the given path.
      def self.configure(app, opts=OPTS)
        app.opts[:heartbeat_path] = (opts[:path] || app.opts[:heartbeat_path] || "/heartbeat").dup.freeze
      end

      module InstanceMethods
        # If the request is for a heartbeat path, return the heartbeat response.
        def call
          if env[PATH_INFO] == opts[:heartbeat_path]
            response = HEARTBEAT_RESPONSE.dup
            response[1] = Hash[response[1]]
            response
          else
            super
          end
        end
      end
    end

    register_plugin(:heartbeat, Heartbeat)
  end
end

