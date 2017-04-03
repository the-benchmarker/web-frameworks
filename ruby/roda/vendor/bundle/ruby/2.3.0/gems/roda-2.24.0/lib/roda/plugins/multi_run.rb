# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The multi_run plugin provides the ability to easily dispatch to other
    # rack applications based on the request path prefix.
    # First, load the plugin:
    #
    #   class App < Roda
    #     plugin :multi_run
    #   end
    #
    # Then, other rack applications can register with the multi_run plugin:
    #
    #   App.run "ra", PlainRackApp
    #   App.run "ro", OtherRodaApp
    #   App.run "si", SinatraApp
    #
    # Inside your route block, you can call r.multi_run to dispatch to all
    # three rack applications based on the prefix:
    #
    #   App.route do |r|
    #     r.multi_run
    #   end
    #
    # This will dispatch routes starting with +/ra+ to +PlainRackApp+, routes
    # starting with +/ro+ to +OtherRodaApp+, and routes starting with +/si+ to
    # SinatraApp.
    #
    # You can pass a block to +multi_run+ that will be called with the prefix,
    # before dispatching to the rack app:
    #
    #   App.route do |r|
    #     r.multi_run do |prefix|
    #       # do something based on prefix before the request is passed further
    #     end
    #   end
    #
    # This is useful for modifying the environment before passing it to the rack app.
    #
    # The multi_run plugin is similar to the multi_route plugin, with the difference
    # being the multi_route plugin keeps all routing subtrees in the same Roda app/class,
    # while multi_run dispatches to other rack apps.  If you want to isolate your routing
    # subtrees, multi_run is a better approach, but it does not let you set instance
    # variables in the main Roda app and have those instance variables usable in
    # the routing subtrees.
    module MultiRun
      # Initialize the storage for the dispatched applications
      def self.configure(app)
        app.opts[:multi_run_apps] ||= {}
      end

      module ClassMethods
        # Freeze the multi_run apps so that there can be no thread safety issues at runtime.
        def freeze
          opts[:multi_run_apps].freeze
          super
        end

        # Hash storing rack applications to dispatch to, keyed by the prefix
        # for the application.
        def multi_run_apps
          opts[:multi_run_apps]
        end

        # Add a rack application to dispatch to for the given prefix when
        # r.multi_run is called.
        def run(prefix, app)
          multi_run_apps[prefix.to_s] = app
          self::RodaRequest.refresh_multi_run_regexp!
        end
      end

      module RequestClassMethods
        # Refresh the multi_run_regexp, using the stored route prefixes,
        # preferring longer routes before shorter routes.
        def refresh_multi_run_regexp!
          @multi_run_regexp = /(#{Regexp.union(roda_class.multi_run_apps.keys.sort.reverse)})/
        end

        # Refresh the multi_run_regexp if it hasn't been loaded yet.
        def multi_run_regexp
          @multi_run_regexp || refresh_multi_run_regexp!
        end
      end

      module RequestMethods
        # If one of the stored route prefixes match the current request,
        # dispatch the request to the stored rack application.
        def multi_run
          on self.class.multi_run_regexp do |prefix|
            yield prefix if block_given?
            run scope.class.multi_run_apps[prefix]
          end
        end
      end
    end

    register_plugin(:multi_run, MultiRun)
  end
end
