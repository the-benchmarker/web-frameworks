# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The response_request plugin gives the response access to the
    # related request instance via the #request method.
    #
    # Example:
    #
    #   plugin :response_request
    module ResponseRequest
      module InstanceMethods
        # Set the response's request to the current request.
        def initialize(env)
          super
          @_response.request = @_request
        end
      end

      module ResponseMethods
        # The request related to this response.
        attr_accessor :request
      end
    end

    register_plugin(:response_request, ResponseRequest)
  end
end
