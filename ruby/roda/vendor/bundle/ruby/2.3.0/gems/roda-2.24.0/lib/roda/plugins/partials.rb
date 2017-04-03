# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The partials plugin adds a +partial+ method, which renders 
    # templates without the layout.
    # 
    #   plugin :partials, :views => 'path/2/views'
    # 
    # Template files are prefixed with an underscore:
    #
    #   partial('test')     # uses _test.erb
    #   partial('dir/test') # uses dir/_test.erb
    #
    # This is basically equivalent to:
    #
    #   render('_test')
    #   render('dir/_test')
    #
    # Note that this plugin automatically loads the :render plugin.
    module Partials
      OPTS = {}.freeze
      SLASH = '/'.freeze

      # Depend on the render plugin, since this overrides
      # some of its methods.
      def self.load_dependencies(app, opts=OPTS)
        app.plugin :render, opts
      end

      module InstanceMethods
        # Renders the given template without a layout, but
        # prefixes the template filename to use with an 
        # underscore.
        def partial(template, opts=OPTS)
          opts = parse_template_opts(template, opts)
          if opts[:template]
            template = opts[:template].split(SLASH)
            template[-1] = "_#{template[-1]}"
            opts[:template] = template.join(SLASH)
          end
          render_template(opts)
        end
      end
    end

    register_plugin(:partials, Partials)
  end
end
