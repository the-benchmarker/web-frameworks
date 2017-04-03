# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The precompile_templates plugin adds support for precompiling template code.
    # This can result in a large memory savings for applications that have large
    # templates or a large number of small templates if the application uses a
    # forking webserver.  By default, template compilation is lazy, so all the
    # child processes in a forking webserver will have their own copy of the
    # compiled template.  By using the precompile_templates plugin, you can
    # precompile the templates in the parent process before forking, and then
    # all of the child processes can use the same precompiled templates, which
    # saves memory.
    #
    # After loading the plugin, you can call +precompile_templates+ with
    # the pattern of templates you would like to precompile:
    #
    #   plugin :precompile_templates
    #   precompile_templates "views/\*\*/*.erb"
    #
    # That will precompile all erb template files in the views directory or
    # any subdirectory.
    #
    # If the templates use local variables, you need to specify which local
    # variables to precompile, which should be an array of symbols:
    #
    #   precompile_templates 'views/users/_*.erb', :locals=>[:user]
    #
    # Note that if you have multiple local variables and are not using a Tilt
    # version greater than 2.0.1, you should specify the :locals option in the
    # same order as the keys in the :locals hash you pass to render/view.  Since
    # hashes are not ordered in ruby 1.8, you should not attempt to precompile
    # templates that use :locals on ruby 1.8 unless you are using a Tilt version
    # greater than 2.0.1.  If you are running the Tilt master branch, you can
    # force sorting of locals using the +:sort_locals+ option when loading the
    # plugin.
    #
    # You can specify other render options when calling +precompile_templates+,
    # including +:cache_key+, +:template_class+, and +:template_opts+.  If you
    # are passing any of those options to render/view for the template, you
    # should pass the same options when precompiling the template.
    #
    # To compile inline templates, just pass a single hash containing an :inline
    # to +precompile_templates+:
    #
    #   precompile_templates :inline=>some_template_string
    module PrecompileTemplates
      OPTS = {}.freeze

      # Load the render plugin as precompile_templates depends on it.
      # Default to sorting the locals if the Tilt version is greater than 2.0.1.
      def self.load_dependencies(app, opts=OPTS)
        app.plugin :render
        app.opts[:precompile_templates_sort] = opts.fetch(:sort_locals, Tilt::VERSION > '2.0.1')
      end

      module ClassMethods
        # Precompile the templates using the given options.  See PrecompileTemplates
        # for details.
        def precompile_templates(pattern, opts=OPTS)
          if pattern.is_a?(Hash)
            opts = pattern.merge(opts)
          end

          locals = opts[:locals] || []
          if locals && self.opts[:precompile_templates_sort]
            locals = locals.sort{|x,y| x.to_s <=> y.to_s}
          end

          compile_opts = if pattern.is_a?(Hash)
            [opts]
          else
            Dir[pattern].map{|file| opts.merge(:path=>File.expand_path(file, nil))}
          end

          instance = allocate
          compile_opts.each do |compile_opt|
            template = instance.send(:retrieve_template, compile_opt)
            template.send(:compiled_method, locals)
          end

          nil
        end
      end
    end

    register_plugin(:precompile_templates, PrecompileTemplates)
  end
end
