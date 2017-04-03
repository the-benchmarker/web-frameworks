# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The status_handler plugin adds a +status_handler+ method which sets a
    # block that is called whenever a response with the relevant response code
    # with an empty body would be returned.
    #
    # This plugin does not support providing the blocks with the plugin call;
    # you must provide them to status_handler calls afterwards:
    #
    #   plugin :status_handler
    #
    #   status_handler(403) do
    #     "You are forbidden from seeing that!"
    #   end
    #   status_handler(404) do
    #     "Where did it go?"
    #   end
    #
    # Before a block is called, any existing headers on the response will be
    # cleared.  So if you want to be sure the headers are set even in your block,
    # you need to reset them in the block.
    module StatusHandler
      def self.configure(app)
        app.opts[:status_handler] ||= {}
      end

      module ClassMethods
        # Install the given block as a status handler for the given HTTP response code.
        def status_handler(code, &block)
          opts[:status_handler][code] = block
        end

        # Freeze the hash of status handlers so that there can be no thread safety issues at runtime.
        def freeze
          opts[:status_handler].freeze
          super
        end
      end

      module InstanceMethods
        # If routing returns a response we have a handler for, call that handler.
        def call
          result = super

          if (block = opts[:status_handler][result[0]]) && (v = result[2]).is_a?(Array) && v.empty?
            @_response.headers.clear
            super(&block)
          else
            result
          end
        end
      end
    end

    register_plugin(:status_handler, StatusHandler)
  end
end
