# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The delay_build plugin does not build the rack app until
    # Roda.app is called, and only rebuilds the rack app if Roda.build!
    # is called.  This differs from Roda's default behavior, which
    # rebuilds the rack app every time the route block changes and
    # every time middleware is added if a route block has already
    # been defined.
    #
    # If you are loading hundreds of middleware after a
    # route block has already been defined, this can fix a possible
    # performance issue, turning an O(n^2) calculation into an
    # O(n) calculation, where n is the number of middleware used.
    module DelayBuild
      module ClassMethods
        # If the app is not been defined yet, build the app.
        def app
          @app || build!
        end

        # Rebuild the application.
        def build!
          @build_app = true
          build_rack_app
          @app
        ensure
          @build_app = false
        end

        private

        # Do not build the rack app automatically, wait for an
        # explicit call to build!.
        def build_rack_app
          super if @build_app
        end
      end
    end

    register_plugin(:delay_build, DelayBuild)
  end
end
