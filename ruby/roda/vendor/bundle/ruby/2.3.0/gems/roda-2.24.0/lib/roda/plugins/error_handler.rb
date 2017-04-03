# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The error_handler plugin adds an error handler to the routing,
    # so that if routing the request raises an error, a nice error
    # message page can be returned to the user.
    # 
    # You can provide the error handler as a block to the plugin:
    #
    #   plugin :error_handler do |e|
    #     "Oh No!"
    #   end
    #
    # Or later via the +error+ class method:
    #
    #   plugin :error_handler
    #
    #   error do |e|
    #     "Oh No!"
    #   end
    #
    # In both cases, the exception instance is passed into the block,
    # and the block can return the request body via a string.
    #
    # If an exception is raised, a new response will be used, with the
    # default status set to 500, before executing the error handler.
    # The error handler can change the response status if necessary,
    # as well set headers and/or write to the body, just like a regular
    # request.
    #
    #
    # By default, this plugin handles StandardError and ScriptError.
    # To override the exception classes it will handle, pass a :classes
    # option to the plugin:
    #
    #   plugin :error_handler, :classes=>[StandardError, ScriptError, NoMemoryError]
    module ErrorHandler
      DEFAULT_ERROR_HANDLER_CLASSES = [StandardError, ScriptError].freeze

      # If a block is given, automatically call the +error+ method on
      # the Roda class with it.
      def self.configure(app, opts={}, &block)
        app.opts[:error_handler_classes] = (opts[:classes] || app.opts[:error_handler_classes] || DEFAULT_ERROR_HANDLER_CLASSES).dup.freeze

        if block
          app.error(&block)
        end
      end

      module ClassMethods
        # Install the given block as the error handler, so that if routing
        # the request raises an exception, the block will be called with
        # the exception in the scope of the Roda instance.
        def error(&block)
          define_method(:handle_error, &block)
          private :handle_error
        end
      end

      module InstanceMethods
        # If an error occurs, set the response status to 500 and call
        # the error handler.
        def call
          super
        rescue *opts[:error_handler_classes] => e
          res = @_response = self.class::RodaResponse.new
          res.status = 500
          super{handle_error(e)}
        end

        private

        # By default, have the error handler reraise the error, so using
        # the plugin without installing an error handler doesn't change
        # behavior.
        def handle_error(e)
          raise e
        end
      end
    end

    register_plugin(:error_handler, ErrorHandler)
  end
end
