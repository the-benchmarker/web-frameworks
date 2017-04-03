# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The module_include plugin adds request_module and response_module class methods
    # for adding modules/methods to request/response classes.  It's designed to make
    # it easier to add request/response methods for a given roda class.  To add a module
    # to the request or response class:
    #
    #   Roda.request_module SomeRequestModule
    #   Roda.response_module SomeResponseModule
    #
    # Alternatively, you can pass a block to the methods and it will create a module
    # automatically:
    #
    #   Roda.request_module do
    #     def description
    #       "#{request_method} #{path_info}"
    #     end
    #   end
    module ModuleInclude
      module ClassMethods
        # Include the given module in the request class. If a block
        # is provided instead of a module, create a module using the
        # the block. Example:
        #
        #   Roda.request_module SomeModule
        #
        #   Roda.request_module do
        #     def description
        #       "#{request_method} #{path_info}"
        #     end
        #   end
        #
        #   Roda.route do |r|
        #     r.description
        #   end
        def request_module(mod = nil, &block)
          module_include(:request, mod, &block)
        end
    
        # Include the given module in the response class. If a block
        # is provided instead of a module, create a module using the
        # the block. Example:
        #
        #   Roda.response_module SomeModule
        #
        #   Roda.response_module do
        #     def error!
        #       self.status = 500
        #     end
        #   end
        #
        #   Roda.route do |r|
        #     response.error!
        #   end
        def response_module(mod = nil, &block)
          module_include(:response, mod, &block)
        end

        private

        # Backbone of the request_module and response_module methods.
        def module_include(type, mod)
          if type == :response
            klass = self::RodaResponse
            iv = :@response_module
          else
            klass = self::RodaRequest
            iv = :@request_module
          end

          if mod
            raise RodaError, "can't provide both argument and block to response_module" if block_given?
            klass.send(:include, mod)
          else
            if instance_variable_defined?(iv)
              mod = instance_variable_get(iv)
            else
              mod = instance_variable_set(iv, Module.new)
              klass.send(:include, mod)
            end

            mod.module_eval(&Proc.new) if block_given?
          end

          mod
        end
      end
    end

    register_plugin(:module_include, ModuleInclude)
  end
end
