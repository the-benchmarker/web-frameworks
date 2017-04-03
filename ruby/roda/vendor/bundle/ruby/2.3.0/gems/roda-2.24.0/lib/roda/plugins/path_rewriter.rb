# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The path_rewriter plugin allows you to rewrite the remaining path
    # or the path info for requests.  This is useful if you want to
    # transparently treat some paths the same as other paths.
    #
    # By default, +rewrite_path+ will rewrite just the remaining path.  So
    # only routing in the current Roda app will be affected.  This is useful
    # if you have other code in your app that uses PATH_INFO and needs to
    # see the original PATH_INFO (for example, when using relative links).
    #
    #   rewrite_path '/a', '/b'
    #   # PATH_INFO '/a' => remaining_path '/b'
    #   # PATH_INFO '/a/c' => remaining_path '/b/c'
    #
    # In some cases, you may want to override PATH_INFO for the rewritten
    # paths, such as when you are passing the request to another Rack app.
    # For those cases, you can use the <tt>:path_info => true</tt> option to
    # +rewrite_path+.
    #
    #   rewrite_path '/a', '/b', :path_info => true
    #   # PATH_INFO '/a' => PATH_INFO '/b'
    #   # PATH_INFO '/a/c' => PATH_INFO '/b/c'
    #
    # If you pass a string to +rewrite_path+, it will rewrite all paths starting
    # with that string.  You can provide a regexp if you want more complete control,
    # such as only matching exact paths.
    #
    #   rewrite_path /\A\/a\z/, '/b'
    #   # PATH_INFO '/a' => remaining_path '/b'
    #   # PATH_INFO '/a/c' => remaining_path '/a/c', no change
    #
    # Patterns can be rewritten dynamically by providing a block accepting a MatchData
    # object and evaluating to the replacement.
    #
    #   rewrite_path(/\A\/a/(\w+)/){|match| "/a/#{match[1].capitalize}"}
    #   # PATH_INFO '/a/moo' => remaining_path '/a/Moo'
    #   rewrite_path(/\A\/a/(\w+)/, :path_info => true){|match| "/a/#{match[1].capitalize}"}
    #   # PATH_INFO '/a/moo' => PATH_INFO '/a/Moo'
    #
    # All path rewrites are applied in order, so if a path is rewritten by one rewrite,
    # it can be rewritten again by a later rewrite.  Note that PATH_INFO rewrites are
    # processed before remaining_path rewrites.
    module PathRewriter
      PATH_INFO = 'PATH_INFO'.freeze
      OPTS={}.freeze

      def self.configure(app)
        app.instance_exec do
          app.opts[:remaining_path_rewrites] ||= []
          app.opts[:path_info_rewrites] ||= []
        end
      end

      module ClassMethods
        # Freeze the path rewrite metadata.
        def freeze
          opts[:remaining_path_rewrites].freeze
          opts[:path_info_rewrites].freeze
          super
        end

        # Record a path rewrite from path +was+ to path +is+.  Options:
        # :path_info :: Modify PATH_INFO, not just remaining path.
        def rewrite_path(was, is = nil, opts=OPTS, &block)
          if is.is_a? Hash
            raise RodaError, "cannot provide two hashes to rewrite_path" unless opts.empty?
            opts = is
            is = nil
          end

          if block
            raise RodaError, "cannot provide both block and string replacement to rewrite_path" if is
            is = block
          end

          was = /\A#{Regexp.escape(was)}/ unless was.is_a?(Regexp)
          array = @opts[opts[:path_info] ? :path_info_rewrites : :remaining_path_rewrites]
          array << [was, is.dup.freeze].freeze
        end
      end

      module RequestMethods
        # Rewrite remaining_path and/or PATH_INFO based on the path rewrites.
        def initialize(scope, env)
          path_info = env[PATH_INFO]

          rewrite_path(scope.class.opts[:path_info_rewrites], path_info)
          super
          remaining_path = @remaining_path = @remaining_path.dup
          rewrite_path(scope.class.opts[:remaining_path_rewrites], remaining_path)
        end

        private

        # Rewrite the given path using the given replacements.
        def rewrite_path(replacements, path)
          replacements.each do |was, is|
            if is.is_a?(Proc)
              path.sub!(was){is.call($~)}
            else
              path.sub!(was, is)
            end
          end
        end
      end
    end

    register_plugin(:path_rewriter, PathRewriter)
  end
end
