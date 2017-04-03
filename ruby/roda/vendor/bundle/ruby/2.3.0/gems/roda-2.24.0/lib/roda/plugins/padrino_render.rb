# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The padrino_render plugin adds rendering support that is
    # similar to Padrino's.  While not everything Padrino's
    # rendering supports is supported by this plugin (yet), it
    # currently handles enough to be a drop in replacement for
    # some applications.
    # 
    #   plugin :padrino_render, :views => 'path/2/views'
    # 
    # Most notably, this makes the +render+ method default to
    # using the layout, similar to how the +view+ method works
    # in the render plugin.  If you want to call render and not
    # use a layout, you can use the <tt>:layout=>false</tt>
    # option:
    #
    #   render('test')                 # layout
    #   render('test', :layout=>false) # no layout
    #
    # Note that this plugin loads the :partials plugin.
    module PadrinoRender
      OPTS = {}.freeze

      # Depend on the render plugin, since this overrides
      # some of its methods.
      def self.load_dependencies(app, opts=OPTS)
        app.plugin :partials, opts
      end

      module InstanceMethods
        # Call view with the given arguments, so that render
        # uses a layout by default.
        def render(template, opts=OPTS)
          view(template, opts)
        end
      end
    end

    register_plugin(:padrino_render, PadrinoRender)
  end
end
