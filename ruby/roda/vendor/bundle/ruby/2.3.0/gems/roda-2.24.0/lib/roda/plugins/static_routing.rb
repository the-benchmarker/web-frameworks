# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The static_routing plugin adds static_* routing class methods for handling
    # static routes (i.e. routes with static paths, no placeholders).  These
    # routes are processed before the normal routing tree and designed for
    # maximum performance.  This can be substantially faster than Roda's normal
    # tree based routing if you have large numbers of static routes, about 3-4x
    # for 100-10000 static routes.  Example:
    #
    #   plugin :static_routing
    #   
    #   static_route '/foo' do |r|
    #     @var = :foo
    #
    #     r.get do
    #       'Not actually reached'
    #     end
    #
    #     r.post{'static POST /#{@var}'}
    #   end
    #
    #   static_get '/foo' do |r|
    #     'static GET /foo'
    #   end
    #
    #   route do |r|
    #     'Not a static route'
    #   end
    #
    # A few things to note in the above example.  First, unlike most other
    # routing methods in Roda, these take the full path of the request, and only
    # match if r.path_info matches exactly.  This is why you need to include the
    # leading slash in the path argument.
    #
    # Second, the static_* routing methods only take a single string argument for
    # the path, they do not acccept other options, and do not handle placeholders
    # in strings.  For any routes needing placeholders, you should use Roda's
    # routing tree.
    #
    # There are separate static_* methods for each type of request method, and these
    # request method specific routes are tried first.  There is also a static_route
    # method that will match regardless of the request method, if there is no
    # matching request methods specific route.  This is why the static_get
    # method call takes precedence over the static_route method call for /foo.
    # As shown above, you can use Roda's routing tree methods inside the
    # static_route block to have shared behavior for different request methods,
    # while still having handling the request methods differently.
    #
    # Note that if you want to use the static_routing plugin and the hooks
    # plugin at the same time, you should load the hooks plugin first.
    module StaticRouting
      def self.configure(app)
        app.opts[:static_routes] = {}
      end

      module ClassMethods
        # Freeze the static route metadata when freezing the app.
        def freeze
          opts[:static_routes].freeze
          opts[:static_routes].each_value(&:freeze)
          super
        end

        # Duplicate static route metadata in subclass.
        def inherited(subclass)
          super
          static_routes = subclass.opts[:static_routes]
          opts[:static_routes].each do |k, v|
            static_routes[k] = v.dup
          end
        end

        # Add a static route for any request method.  These are
        # tried after the request method specific static routes (e.g.
        # static_get), but allow you to use Roda's routing tree
        # methods inside the route for handling shared behavior while
        # still allowing request method specific handling.
        def static_route(path, &block)
          add_static_route(nil, path, &block)
        end
        
        # Return the static route for the given request method and path.
        def static_route_for(method, path)
          if h = opts[:static_routes][path]
            h[method] || h[nil]
          end
        end

        [:get, :post, :delete, :head, :options, :link, :patch, :put, :trace, :unlink].each do |meth|
          request_method = meth.to_s.upcase
          define_method("static_#{meth}") do |path, &block|
            add_static_route(request_method, path, &block)
          end
        end

        private

        # Add a static route for the given method.
        def add_static_route(method, path, &block)
          (opts[:static_routes][path] ||= {})[method] = block
        end
      end

      module InstanceMethods
        # If there is a static routing method for the given path, call it
        # instead having the routing tree handle the request.
        def call(&block)
          super do |r|
            if route = self.class.static_route_for(r.request_method, r.path_info)
              r.static_route(&route)
            else
              instance_exec(r, &block)
            end
          end
        end
      end

      module RequestMethods
        # Assume that this request matches a static route, setting
        # the remaining path to the emptry string and passing
        # control to the given block.
        def static_route(&block)
          @remaining_path = ''

          always do
            scope.instance_exec(self, &block)
          end
        end
      end
    end

    register_plugin(:static_routing, StaticRouting)
  end
end
