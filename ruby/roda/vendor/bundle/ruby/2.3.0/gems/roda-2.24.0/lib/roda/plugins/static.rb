# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The static plugin loads the Rack::Static middleware into the application.
    # It mainly exists to make serving static files simpler, by supplying
    # defaults to Rack::Static that are appropriate for Roda.
    #
    # The static plugin recognizes the application's :root option, and by default
    # sets the Rack::Static +:root+ option to the +public+ subfolder of the application's
    # +:root+ option.  Additionally, if a relative path is provided as the :root
    # option to the plugin, it will be considered relative to the application's
    # +:root+ option.
    #
    # Since the :urls option for Rack::Static is always required, the static plugin
    # uses a separate option for it.
    #
    # Users of this plugin may want to consider using the public plugin instead.
    # 
    # Examples:
    #
    #   opts[:root] = '/path/to/app'
    #   plugin :static, ['/js', '/css'] # path: /path/to/app/public
    #   plugin :static, ['/images'], :root=>'pub'  # path: /path/to/app/pub
    #   plugin :static, ['/media'], :root=>'/path/to/public' # path: /path/to/public
    module Static
      # Load the Rack::Static middleware.  Use the paths given as the :urls option,
      # and set the :root option to be relative to the application's :root option.
      def self.configure(app, paths, opts={})
        opts = opts.dup
        opts[:urls] = paths
        opts[:root] =  app.expand_path(opts[:root]||"public")
        app.use ::Rack::Static, opts
      end
    end

    register_plugin(:static, Static)
  end
end
