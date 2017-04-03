# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The not_found plugin adds a +not_found+ class method which sets
    # a block that is called whenever a 404 response with an empty body
    # would be returned.  The usual use case for this is the desire for
    # nice error pages if the page is not found.
    #
    # You can provide the block with the plugin call:
    #
    #   plugin :not_found do
    #     "Where did it go?"
    #   end
    #   
    # Or later via a separate call to +not_found+:
    #
    #   plugin :not_found
    #
    #   not_found do
    #     "Where did it go?"
    #   end
    #
    # Before not_found is called, any existing headers on the response
    # will be cleared.  So if you want to be sure the headers are set
    # even in a not_found block, you need to reset them in the
    # not_found block.
    #
    # This plugin is now a wrapper around the +status_handler+ plugin and
    # still exists mainly for backward compatibility.
    module NotFound
      # Require the status_handler plugin
      def self.load_dependencies(app)
        app.plugin :status_handler
      end

      # If a block is given, install the block as the not_found handler.
      def self.configure(app, &block)
        if block
          app.not_found(&block)
        end
      end

      module ClassMethods
        # Install the given block as the not_found handler.
        def not_found(&block)
          status_handler(404, &block)
        end
      end
    end

    register_plugin(:not_found, NotFound)
  end
end
