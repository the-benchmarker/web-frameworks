# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The path plugin adds support for named paths.  Using the +path+ class method, you can
    # easily create <tt>*_path</tt> instance methods for each named path.  Those instance
    # methods can then be called if you need to get the path for a form action, link,
    # redirect, or anything else.
    #
    # Additionally, you can call the +path+ class method with a class and a block, and it will register
    # the class.  You can then call the +path+ instance method with an instance of that class, and it will
    # instance_exec the block with the arguments provided to path.
    #
    # Example:
    #
    #   plugin :path
    #   path :foo, '/foo'
    #   path :bar do |bar|
    #     "/bar/#{bar.id}"
    #   end
    #   path Baz do |baz, *paths|
    #     "/baz/#{baz.id}/#{paths.join('/')}"
    #   end
    #   path Quux do |quux, path|
    #     "/quux/#{quux.id}/#{path}"
    #   end
    #
    #   route do |r|
    #     r.post 'foo' do
    #       r.redirect foo_path # /foo
    #     end
    #
    #     r.post 'bar' do
    #       bar = Bar.create(r.params['bar'])
    #       r.redirect bar_path(bar) # /bar/1
    #     end
    #
    #     r.post 'baz' do
    #       bar = Baz[1]
    #       r.redirect path(baz, 'c', 'd') # /baz/1/c/d
    #     end
    #
    #     r.post 'quux' do
    #       bar = Quux[1]
    #       r.redirect path(quux, '/bar') # /quux/1/bar
    #     end
    #   end
    #
    # The path method accepts the following options when not called with a class:
    #
    # :add_script_name :: Prefix the path generated with SCRIPT_NAME. This defaults to the app's
    #                     :add_script_name option.
    # :name :: Provide a different name for the method, instead of using <tt>*_path</tt>.
    # :url :: Create a url method in addition to the path method, which will prefix the string generated
    #         with the appropriate scheme, host, and port.  If true, creates a <tt>*_url</tt>
    #         method.  If a Symbol or String, uses the value as the url method name.
    # :url_only :: Do not create a path method, just a url method.
    #
    # Note that if :add_script_name, :url, or :url_only is used, will also create a <tt>_*_path</tt>
    # method.  This is necessary in order to support path methods that accept blocks, as you can't pass
    # a block to a block that is instance_execed.
    module Path
      DEFAULT_PORTS = {'http' => 80, 'https' => 443}.freeze
      OPTS = {}.freeze

      # Initialize the path classes when loading the plugin. Options:
      # :by_name :: Register classes by name, which is friendlier when reloading code (defaults to
      #             true in development mode)
      def self.configure(app, opts=OPTS)
        app.instance_eval do
          self.opts[:path_class_by_name] = opts.fetch(:by_name, ENV['RACK_ENV'] == 'development')
          self.opts[:path_classes] ||= {}
          unless path_block(String)
            path(String){|str| str}
          end
        end
      end

      module ClassMethods
        # Hash of recognizes classes for path instance method.  Keys are classes, values are procs.
        def path_classes
          opts[:path_classes]
        end

        # Freeze the path classes when freezing the app.
        def freeze
          path_classes.freeze
          super
        end

        # Create a new instance method for the named path.  See plugin module documentation for options.
        def path(name, path=nil, opts=OPTS, &block)
          if name.is_a?(Class)
            raise RodaError, "can't provide path or options when calling path with a class" unless path.nil? && opts.empty?
            raise RodaError, "must provide a block when calling path with a class" unless block
            if self.opts[:path_class_by_name]
              name = name.name
            end
            path_classes[name] = block
            return
          end

          if path.is_a?(Hash)
            raise RodaError,  "cannot provide two option hashses to Roda.path" unless opts.empty?
            opts = path
            path = nil
          end

          raise RodaError,  "cannot provide both path and block to Roda.path" if path && block
          raise RodaError,  "must provide either path or block to Roda.path" unless path || block

          if path
            path = path.dup.freeze
            block = lambda{path}
          end

          meth = opts[:name] || "#{name}_path"
          url = opts[:url]
          add_script_name = opts.fetch(:add_script_name, self.opts[:add_script_name])

          if add_script_name || url || opts[:url_only]
            _meth = "_#{meth}"
            define_method(_meth, &block)
          end

          unless opts[:url_only]
            if add_script_name
              define_method(meth) do |*a, &blk|
                request.script_name.to_s + send(_meth, *a, &blk)
              end
            else
              define_method(meth, &block)
            end
          end

          if url || opts[:url_only]
            url_meth = if url.is_a?(String) || url.is_a?(Symbol)
              url
            else
              "#{name}_url"
            end

            url_block = lambda do |*a, &blk|
              r = request
              scheme = r.scheme
              port = r.port
              uri = ["#{scheme}://#{r.host}#{":#{port}" unless DEFAULT_PORTS[scheme] == port}"]
              uri << request.script_name.to_s if add_script_name
              uri << send(_meth, *a, &blk)
              File.join(uri)
            end

            define_method(url_meth, &url_block)
          end

          nil
        end
        
        # Return the block related to the given class, or nil if there is no block.
        def path_block(klass)
          if opts[:path_class_by_name]
            klass = klass.name
          end
          path_classes[klass]
        end
      end

      module InstanceMethods
        # Return a path based on the class of the object.  The object passed must have
        # had its class previously registered with the application.  If the app's
        # :add_script_name option is true, this prepends the SCRIPT_NAME to the path.
        def path(obj, *args)
          app = self.class
          unless blk = app.path_block(obj.class)
            raise RodaError, "unrecognized object given to Roda#path: #{obj.inspect}"
          end

          path = instance_exec(obj, *args, &blk)
          path = request.script_name.to_s + path if app.opts[:add_script_name]
          path
        end
      end
    end

    register_plugin(:path, Path)
  end
end
