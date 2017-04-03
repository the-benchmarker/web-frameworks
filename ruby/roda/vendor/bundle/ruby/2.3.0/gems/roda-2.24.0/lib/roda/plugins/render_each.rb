# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The render_each plugin allows you to render a template for each
    # value in an enumerable, returning the concatention of all of the
    # template renderings.  For example:
    #
    #   render_each([1,2,3], :foo)
    #
    # will render the +foo+ template 3 times.  Each time the template
    # is rendered, the local variable +foo+ will contain the given
    # value (e.g. on the first rendering +foo+ is 1).
    #
    # You can pass additional render options via an options hash:
    #
    #   render_each([1,2,3], :foo, :views=>'partials')
    #
    # One additional option supported by is +:local+, which sets the
    # local variable containing the current value to use.  So:
    #
    #   render_each([1,2,3], :foo, :local=>:bar)
    #
    # Will render the +foo+ template, but the local variable used inside
    # the template will be +bar+.  You can use <tt>:local=>nil</tt> to
    # not set a local variable inside the template.
    module RenderEach
      OPTS = {}.freeze

      # Load the render plugin before this plugin, since this plugin
      # calls the render method.
      def self.load_dependencies(app)
        app.plugin :render
      end

      module InstanceMethods
        # For each value in enum, render the given template using the
        # given opts.  The template and options hash are passed to +render+.
        # Additional options supported:
        # :local :: The local variable to use for the current enum value
        #           inside the template.  An explicit +nil+ value does not
        #           set a local variable.  If not set, uses the template name.
        def render_each(enum, template, opts=OPTS)
          if as = opts.has_key?(:local)
            as = opts[:local]
          else
            as = template.to_s.to_sym
          end

          if as
            opts = opts.dup
            if locals = opts[:locals]
              locals = opts[:locals] = Hash[locals]
            else
              locals = opts[:locals] = {}
            end
          end

          enum.map do |v|
            locals[as] = v if as
            render_template(template, opts)
          end.join
        end
      end
    end

    register_plugin(:render_each, RenderEach)
  end
end
