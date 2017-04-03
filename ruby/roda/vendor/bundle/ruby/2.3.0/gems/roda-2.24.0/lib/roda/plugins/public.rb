# frozen-string-literal: true

require 'uri'

#
class Roda
  module RodaPlugins
    # The public plugin adds a +r.public+ routing method to serve static files
    # from a directory.
    #
    # The public plugin recognizes the application's :root option, and defaults to
    # using the +public+ subfolder of the application's +:root+ option.  If the application's
    # :root option is not set, it defaults to the the +public+ folder in the working
    # directory.  Additionally, if a relative path is provided as the :root
    # option to the plugin, it will be considered relative to the application's
    # +:root+ option.
    #
    # Examples:
    #
    #   opts[:root] = '/path/to/app'
    #   plugin :public
    #   plugin :public, :root=>'static'
    module Public
      NULL_BYTE = "\0".freeze
      SPLIT = Regexp.union(*[File::SEPARATOR, File::ALT_SEPARATOR].compact)
      PARSER = RUBY_VERSION >= '1.9' ? URI::DEFAULT_PARSER : URI

      # Use options given to setup a Rack::File instance for serving files. Options:
      # :default_mime :: The default mime type to use if the mime type is not recognized.
      # :gzip :: Whether to serve already gzipped files with a .gz extension for clients
      #          supporting gzipped transfer encoding.
      # :headers :: A hash of headers to use for statically served files
      # :root :: Use this option for the root of the public directory (default: "public")
      def self.configure(app, opts={})
        root =  app.expand_path(opts[:root]||"public")
        app.opts[:public_server] = ::Rack::File.new(root, opts[:headers]||{}, opts[:default_mime] || 'text/plain')
        app.opts[:public_gzip] = opts[:gzip]
      end

      module RequestMethods
        # Serve files from the public directory if the file exists and this is a GET request.
        def public
          if is_get?
            path = PARSER.unescape(real_remaining_path)
            return if path.include?(NULL_BYTE)

            roda_opts = roda_class.opts
            server = roda_opts[:public_server]
            path = ::File.join(server.root, *public_path_segments(path))

            if roda_opts[:public_gzip] && env['HTTP_ACCEPT_ENCODING'] =~ /\bgzip\b/
              gzip_path = path + '.gz'

              if public_file_readable?(gzip_path)
                res = public_serve(server, gzip_path)
                headers = res[1]

                if mime_type = ::Rack::Mime.mime_type(::File.extname(path), 'text/plain')
                  headers['Content-Type'] = mime_type
                end
                headers['Content-Encoding'] = 'gzip'

                halt res
              end
            end

            if public_file_readable?(path)
              halt public_serve(server, path)
            end
          end
        end

        private

        # Return an array of segments for the given path, handling ..
        # and . components
        def public_path_segments(path)
          segments = []
            
          path.split(SPLIT).each do |seg|
            next if seg.empty? || seg == '.'
            seg == '..' ? segments.pop : segments << seg
          end
            
          segments
        end

        # Return whether the given path is a readable regular file.
        def public_file_readable?(path)
          ::File.file?(path) && ::File.readable?(path)
        rescue SystemCallError
          # :nocov:
          false
          # :nocov:
        end

        if ::Rack.release > '2'
          # :nocov:
          def public_serve(server, path)
            server.serving(self, path)
          end
          # :nocov:
        else
          # Serve the given path using the given Rack::File server.
          def public_serve(server, path)
            server = server.dup
            server.path = path
            server.serving(env)
          end
        end
      end
    end

    register_plugin(:public, Public)
  end
end
