# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The middleware plugin allows the Roda app to be used as
    # rack middleware.
    #
    # In the example below, requests to /mid will return Mid
    # by the Mid middleware, and requests to /app will not be
    # matched by the Mid middleware, so they will be forwarded
    # to App.
    #
    #   class Mid < Roda
    #     plugin :middleware
    #
    #     route do |r|
    #       r.is "mid" do
    #         "Mid"
    #       end
    #     end
    #   end
    #
    #   class App < Roda
    #     use Mid
    #
    #     route do |r|
    #       r.is "app" do
    #         "App"
    #       end
    #     end
    #   end
    #
    #   run App
    #
    # It is possible to use the Roda app as a regular app even when using
    # the middleware plugin.
    #
    # You can support configurable middleware by passing a block when loading
    # the plugin:
    #
    #   class Mid < Roda
    #     plugin :middleware do |middleware, *args, &block|
    #       middleware.opts[:middleware_args] = args
    #       block.call(middleware)
    #     end
    #
    #     route do |r|
    #       r.is "mid" do
    #         opts[:middleware_args].join(' ')
    #       end
    #     end
    #   end
    #
    #   class App < Roda
    #     use Mid, :foo, :bar do |middleware|
    #       middleware.opts[:middleware_args] << :baz
    #     end
    #   end
    #
    #   # Request to App for /mid returns
    #   # "foo bar baz"
    #
    # Note that when supporting configurable middleware via a block, the middleware
    # used is a subclass of the class loading the plugin, instead of the class itself.
    # This is done so the same class can be used as middleware with multiple separate
    # configurations.
    module Middleware
      # Configure the middleware plugin.  Options:
      # :env_var :: Set the environment variable to use to indicate to the roda
      #             application that the current request is a middleware request.
      #             You should only need to override this if you are using multiple
      #             roda middleware in the same application.
      def self.configure(app, opts={}, &block)
        app.opts[:middleware_env_var] = opts[:env_var] if opts.has_key?(:env_var)
        app.opts[:middleware_env_var] ||= 'roda.forward_next'
        app.opts[:middleware_configure] = block if block
      end

      # Forward instances are what is actually used as middleware.
      class Forwarder
        # Store the current middleware and the next middleware to call.
        def initialize(mid, app, *args, &block)
          @mid = if configure = mid.opts[:middleware_configure]
            mid = Class.new(mid)
            configure.call(mid, *args, &block)
            mid
          else
            raise RodaError, "cannot provide middleware args or block unless loading middleware plugin with a block" if block || !args.empty?
            mid
          end
          @app = app
        end

        # When calling the middleware, first call the current middleware.
        # If this returns a result, return that result directly.  Otherwise,
        # pass handling of the request to the next middleware.
        def call(env)
          res = nil

          call_next = catch(:next) do
            env[@mid.opts[:middleware_env_var]] = true
            res = @mid.call(env)
            false
          end

          if call_next
            @app.call(env)
          else
            res
          end
        end
      end

      module ClassMethods
        # Create a Forwarder instead of a new instance if a non-Hash is given.
        def new(app, *args, &block)
          if app.is_a?(Hash)
            super
          else
            Forwarder.new(self, app, *args, &block)
          end
        end

        # Override the route block so that if no route matches, we throw so
        # that the next middleware is called.
        def route(*args, &block)
          super do |r|
            res = instance_exec(r, &block)
            throw :next, true if r.forward_next
            res
          end
        end
      end

      module RequestMethods
        # Whether to forward the request to the next application.  Set only if
        # this request is being performed for middleware.
        def forward_next
          env[roda_class.opts[:middleware_env_var]]
        end
      end
    end

    register_plugin(:middleware, Middleware)
  end
end
