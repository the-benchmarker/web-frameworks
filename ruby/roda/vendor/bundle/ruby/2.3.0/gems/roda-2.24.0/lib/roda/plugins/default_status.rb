# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The default_status plugin accepts a block which should
    # return a response status integer. This integer will be used as
    # the default response status (usually 200) if the body has been
    # written to, and you have not explicitly set a response status.
    # The block given to the block is instance_execed in the context
    # of the response.
    #
    # Example:
    #
    #   # Use 201 default response status for all requests
    #   plugin :default_status do
    #     201
    #   end
    module DefaultStatus
      def self.configure(app, &block)
        raise RodaError, "default_status plugin requires a block" unless block
        app.opts[:default_status] = block
      end

      module ResponseMethods
        # instance_exec the default_status plugin block to get the response
        # status.
        def default_status
          instance_exec(&roda_class.opts[:default_status])
        end
      end
    end

    register_plugin(:default_status, DefaultStatus)
  end
end
