# frozen-string-literal: true

class Roda
  module RodaPlugins
    # The symbol_status plugin patches the +status=+ response method to
    # accept the status name as a symbol.  If given an integer value,
    # the default behaviour is used.
    #
    # Examples:
    #   r.is "needs_authorization"
    #     response.status = :unauthorized
    #   end
    #   r.is "nothing"
    #     response.status = :no_content
    #   end
    #
    # The conversion is done through <tt>Rack::Utils.status_code</tt>.
    module SymbolStatus
      module ResponseMethods
        # Sets the response status code by fixnum or symbol name
        def status=(code)
          code = Rack::Utils.status_code(code) if code.is_a?(Symbol)
          super(code)
        end
      end
    end

    register_plugin(:symbol_status, SymbolStatus)
  end
end
