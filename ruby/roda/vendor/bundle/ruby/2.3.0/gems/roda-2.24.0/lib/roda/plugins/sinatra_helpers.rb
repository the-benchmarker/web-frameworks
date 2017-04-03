# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The sinatra_helpers plugin ports most of the helper methods
    # defined in Sinatra::Helpers to Roda, other than those
    # helpers that were already covered by other plugins such
    # as caching and streaming.
    #
    # Unlike Sinatra, the helper methods are added to either
    # the request or response classes instead of directly to
    # the scope of the route block.  However, for consistency
    # with Sinatra, delegate methods are added to the scope
    # of the route block that call the methods on the request
    # or response.  If you do not want to pollute the namespace
    # of the route block, you should load the plugin with the
    # :delegate => false option:
    #
    #   plugin :sinatra_helpers, :delegate=>false
    #
    # == Class Methods Added
    #
    # The only class method added by this plugin is +mime_type+,
    # which is a shortcut for retrieving or setting MIME types
    # in Rack's MIME database:
    #
    #   Roda.mime_type 'csv' # => 'text/csv'
    #   Roda.mime_type 'foobar', 'application/foobar' # set
    #
    # == Request Methods Added 
    #
    # In addition to adding the following methods, this changes
    # +redirect+ to use a 303 response status code by default for
    # HTTP 1.1 non-GET requests, and to automatically use
    # absolute URIs if the +:absolute_redirects+ Roda class option
    # is true, and to automatically prefix redirect paths with the
    # script name if the +:prefixed_redirects+ Roda class option is
    # true.
    #
    # When adding delegate methods, a logger method is added to
    # the route block scope that calls the logger method on the request.
    # 
    # === back
    #
    # +back+ is an alias to referrer, so you can do:
    #
    #   redirect back
    #
    # === error
    #
    # +error+ sets the response status code to 500 (or a status code you provide),
    # and halts the request.  It takes an optional body:
    #
    #   error           # 500 response, empty boby
    #   error 501       # 501 reponse, empty body
    #   error 'b'       # 500 response, 'b' body
    #   error 501, 'b'  # 501 response, 'b' body
    #
    # === not_found
    #
    # +not_found+ sets the response status code to 404 and halts the request.
    # It takes an optional body:
    #
    #   not_found      # 404 response, empty body
    #   not_found 'b'  # 404 response, 'b' body
    #
    # === uri
    #
    # +uri+ by default returns absolute URIs that are prefixed
    # by the script name:
    #
    #   request.script_name # => '/foo'
    #   uri '/bar'          # => 'http://example.org/foo/bar'
    #
    # You can turn of the absolute or script name prefixing if you want:
    #
    #   uri '/bar', false        # => '/foo/bar'
    #   uri '/bar', true, false  # => 'http://example.org/bar'
    #   uri '/bar', false, false # => '/bar'
    #
    # This method is aliased as +url+ and +to+.
    #
    # === send_file
    #
    # This will serve the file with the given path from the file system:
    #
    #   send_file 'path/to/file.txt'
    #
    # Options:
    #
    # :disposition :: Set the Content-Disposition to the given disposition.
    # :filename :: Set the Content-Disposition to attachment (unless :disposition is set),
    #              and set the filename parameter to the value.
    # :last_modified :: Explicitly set the Last-Modified header to the given value, and
    #                   return a not modified response if there has not been modified since
    #                   the previous request.  This option requires the caching plugin.
    # :status :: Override the status for the response.
    # :type :: Set the Content-Type to use for this response.
    #
    # == Response Methods Added
    #
    # === body
    #
    # When called with an argument or block, +body+ sets the body, otherwise
    # it returns the body:
    #
    #   body      # => []
    #   body('b') # set body to 'b'
    #   body{'b'} # set body to 'b', but don't call until body is needed
    #
    # === body=
    #
    # +body+ sets the body to the given value:
    #
    #   response.body = 'v'
    #
    # This method is not delegated to the scope of the route block,
    # call +body+ with an argument to set the value.
    #
    # === status
    #
    # When called with an argument, +status+ sets the status, otherwise
    # it returns the status:
    #
    #   status      # => 200
    #   status(301) # sets status to 301
    #
    # === headers
    #
    # When called with an argument, +headers+ merges the given headers
    # into the current headers, otherwise it returns the headers:
    #
    #   headers['Foo'] = 'Bar'
    #   headers 'Foo' => 'Bar'
    #
    # === mime_type
    #
    # +mime_type+ just calls the Roda class method to get the mime_type.
    #
    # === content_type
    #
    # When called with an argument, +content_type+ sets the Content-Type
    # based on the argument, otherwise it returns the Content-Type.
    #
    #   mime_type             # => 'text/html'
    #   mime_type 'csv'       # set Content-Type to 'text/csv'
    #   mime_type :csv        # set Content-Type to 'text/csv'
    #   mime_type '.csv'      # set Content-Type to 'text/csv'
    #   mime_type 'text/csv'  # set Content-Type to 'text/csv'
    #
    # Options:
    #
    # :charset :: Set the charset for the mime type to the given charset, if the charset is
    #             not already set in the mime type.
    # :default :: Uses the given type if the mime type is not known.  If this option is not
    #             used and the mime type is not known, an exception will be raised.
    #
    # === attachment
    #
    # When called with no filename, +attachment+ just sets the Content-Disposition
    # to attachment.  When called with a filename, this sets the Content-Disposition
    # to attachment with the appropriate filename parameter, and if the filename
    # extension is recognized, this also sets the Content-Type to the appropriate
    # MIME type if not already set.
    #
    #   attachment          # set Content-Disposition to 'attachment'
    #   attachment 'a.csv'  # set Content-Disposition to 'attachment;filename="a.csv"',
    #                       # also set Content-Type to 'text/csv'
    #
    # === status predicates
    #
    # This adds the following predicate methods for checking the status:
    #
    #   informational?  # 100-199
    #   success?        # 200-299
    #   redirect?       # 300-399
    #   client_error?   # 400-499
    #   not_found?      # 404
    #   server_error?   # 500-599
    #
    # If the status has not yet been set for the response, these will
    # return +nil+.
    #
    # == License
    #
    # The implementation was originally taken from Sinatra,
    # which is also released under the MIT License:
    #
    # Copyright (c) 2007, 2008, 2009 Blake Mizerany
    # Copyright (c) 2010, 2011, 2012, 2013, 2014 Konstantin Haase
    # 
    # Permission is hereby granted, free of charge, to any person
    # obtaining a copy of this software and associated documentation
    # files (the "Software"), to deal in the Software without
    # restriction, including without limitation the rights to use,
    # copy, modify, merge, publish, distribute, sublicense, and/or sell
    # copies of the Software, and to permit persons to whom the
    # Software is furnished to do so, subject to the following
    # conditions:
    # 
    # The above copyright notice and this permission notice shall be
    # included in all copies or substantial portions of the Software.
    # 
    # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    # EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
    # OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    # NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    # HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    # WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    # FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    # OTHER DEALINGS IN THE SOFTWARE.
    module SinatraHelpers
      CONTENT_TYPE = "Content-Type".freeze
      CONTENT_DISPOSITION = "Content-Disposition".freeze
      CONTENT_LENGTH = "Content-Length".freeze
      OCTET_STREAM = 'application/octet-stream'.freeze
      ATTACHMENT = 'attachment'.freeze
      HTTP_VERSION = 'HTTP_VERSION'.freeze
      HTTP11 = "HTTP/1.1".freeze
      HTTP_X_FORWARDED_HOST = "HTTP_X_FORWARDED_HOST".freeze
      EMPTY_STRING = ''.freeze
      SLASH = '/'.freeze
      SEMICOLON = ';'.freeze
      COMMA = ', '.freeze
      CHARSET = 'charset'.freeze
      OPTS = {}.freeze

      # Add delegate methods to the route block scope
      # calling request or response methods, unless the
      # :delegate option is false.
      def self.configure(app, opts=OPTS)
        app.send(:include, DelegateMethods) unless opts[:delegate] == false
      end

      # Class used when the response body is set explicitly, instead
      # of using Roda's default body array and response.write to
      # write to it.
      class DelayedBody
        # Save the block that will return the body, it won't be
        # called until the body is needed.
        def initialize(&block)
          @block = block
        end

        # If the body is a String, yield it, otherwise yield each string
        # returned by calling each on the body.
        def each
          v = value
          if v.is_a?(String)
            yield v
          else
            v.each{|s| yield s}
          end
        end

        # Assume that if the body has been set directly that it is
        # never empty.
        def empty?
          false
        end

        # Return the body as a single string, mostly useful during testing.
        def join
          a = []
          each{|s| a << s}
          a.join
        end

        # Calculate the length for the body.
        def length
          length = 0
          each{|s| length += s.bytesize}
          length
        end

        private

        # Cache the body returned by the block.  This way the block won't
        # be called multiple times.
        def value
          @value ||= @block.call
        end
      end

      module RequestMethods
        # Alias for referrer
        def back
          referrer
        end

        # Halt processing and return the error status provided with the given code and
        # optional body.
        # If a single argument is given and it is not an integer, consider it the body
        # and use a 500 status code.
        def error(code=500, body = nil)
          unless code.is_a?(Integer)
            body = code
            code = 500
          end

          response.status = code
          response.body = body if body
          halt
        end

        # Halt processing and return a 404 response with an optional body.
        def not_found(body = nil)
          error(404, body)
        end

        # If the absolute_redirects or :prefixed_redirects roda class options has been set, respect those
        # and update the path.
        def redirect(path=(no_add_script_name = true; default_redirect_path), status=default_redirect_status)
          opts = roda_class.opts
          absolute_redirects = opts[:absolute_redirects]
          prefixed_redirects = no_add_script_name ? false : opts[:prefixed_redirects]
          path = uri(path, absolute_redirects, prefixed_redirects) if absolute_redirects || prefixed_redirects
          super(path, status)
        end

        # Use the contents of the file at +path+ as the response body.  See plugin documentation for options.
        def send_file(path, opts = OPTS)
          res = response
          headers = res.headers
          if opts[:type] || !headers[CONTENT_TYPE]
            res.content_type(opts[:type] || ::File.extname(path), :default => OCTET_STREAM)
          end

          disposition = opts[:disposition]
          filename    = opts[:filename]
          if disposition || filename
            disposition ||= ATTACHMENT
            filename = path if filename.nil?
            res.attachment(filename, disposition)
          end

          if lm = opts[:last_modified]
            last_modified(lm)
          end

          file = ::Rack::File.new nil
          s, h, b = if Rack.release > '2'
            # :nocov:
            file.serving(self, path)
            # :nocov:
          else
            file.path = path
            file.serving(@env)
          end

          res.status = opts[:status] || s
          headers.delete(CONTENT_LENGTH)
          headers.replace(h.merge!(headers))
          res.body = b

          halt
        rescue Errno::ENOENT
          not_found
        end

        # Generates the absolute URI for a given path in the app.
        # Takes Rack routers and reverse proxies into account.
        def uri(addr = nil, absolute = true, add_script_name = true)
          addr = addr.to_s if addr
          return addr if addr =~ /\A[A-z][A-z0-9\+\.\-]*:/
          uri = if absolute
            h = if @env.has_key?(HTTP_X_FORWARDED_HOST) || port != (ssl? ? 443 : 80)
              host_with_port
            else
              host
            end
            ["http#{'s' if ssl?}://#{h}"]
          else
            [EMPTY_STRING]
          end
          uri << script_name.to_s if add_script_name
          uri << (addr || path_info)
          File.join(uri)
        end
        alias url uri
        alias to uri

        private

        # Use a 303 response for non-GET responses if client uses HTTP 1.1.
        def default_redirect_status
          if @env[HTTP_VERSION] == HTTP11 && !is_get?
            303
          else
            super
          end
        end
      end

      module ResponseMethods
        # Set or retrieve the response status code.
        def status(value = (return @status; nil))
          @status = value
        end

        # Set or retrieve the response body. When a block is given,
        # evaluation is deferred until the body is needed.
        def body(value = (return @body unless block_given?; nil), &block)
          if block
            @body = DelayedBody.new(&block)
          else
            self.body = value
          end
        end

        # Set the body to the given value.
        def body=(body)
          @body = DelayedBody.new{body}
        end

        # If the body is a DelayedBody, set the appropriate length for it.
        def finish
          @length = @body.length if @body.is_a?(DelayedBody) && !@headers[CONTENT_LENGTH]
          super
        end

        # Set multiple response headers with Hash, or return the headers if no
        # argument is given.
        def headers(hash = (return @headers; nil))
          @headers.merge!(hash)
        end

        # Look up a media type by file extension in Rack's mime registry.
        def mime_type(type)
          roda_class.mime_type(type)
        end

        # Set the Content-Type of the response body given a media type or file
        # extension.  See plugin documentation for options.
        def content_type(type = (return @headers[CONTENT_TYPE]; nil), opts = OPTS)
          unless (mime_type = mime_type(type) || opts[:default])
            raise RodaError, "Unknown media type: #{type}"
          end

          unless opts.empty?
            opts.each do |key, val|
              next if key == :default || (key == :charset && mime_type.include?(CHARSET))
              val = val.inspect if val =~ /[";,]/
              mime_type += "#{mime_type.include?(SEMICOLON) ? COMMA : SEMICOLON}#{key}=#{val}"
            end
          end

          @headers[CONTENT_TYPE] = mime_type
        end

        # Set the Content-Disposition to "attachment" with the specified filename,
        # instructing the user agents to prompt to save.
        def attachment(filename = nil, disposition=ATTACHMENT)
          if filename
            params = "; filename=#{File.basename(filename).inspect}"
            unless @headers[CONTENT_TYPE]
              ext = File.extname(filename)
              unless ext.empty?
                content_type(ext)
              end
            end
          end
          @headers[CONTENT_DISPOSITION] = "#{disposition}#{params}"
        end

        # Whether or not the status is set to 1xx. Returns nil if status not yet set.
        def informational?
          @status.between?(100, 199) if @status
        end

        # Whether or not the status is set to 2xx. Returns nil if status not yet set.
        def success?
          @status.between?(200, 299) if @status
        end

        # Whether or not the status is set to 3xx. Returns nil if status not yet set.
        def redirect?
          @status.between?(300, 399) if @status
        end

        # Whether or not the status is set to 4xx. Returns nil if status not yet set.
        def client_error?
          @status.between?(400, 499) if @status
        end

        # Whether or not the status is set to 5xx. Returns nil if status not yet set.
        def server_error?
          @status.between?(500, 599) if @status
        end

        # Whether or not the status is set to 404. Returns nil if status not yet set.
        def not_found?
          @status == 404 if @status
        end
      end

      module ClassMethods
        # If a type and value are given, set the value in Rack's MIME registry.
        # If only a type is given, lookup the type in Rack's MIME registry and
        # return it.
        def mime_type(type=(return; nil), value = nil)
          return type.to_s if type.to_s.include?(SLASH)
          type = ".#{type}" unless type.to_s[0] == ?.
          if value
            Rack::Mime::MIME_TYPES[type] = value
          else
            Rack::Mime.mime_type(type, nil)
          end
        end
      end

      module DelegateMethods
        [:logger, :back].each do |meth|
          define_method(meth){@_request.send(meth)}
        end
        [:redirect, :uri, :url, :to, :send_file, :error, :not_found].each do |meth|
          define_method(meth){|*v, &block| @_request.send(meth, *v, &block)}
        end

        [:informational?, :success?, :redirect?, :client_error?, :server_error?, :not_found?].each do |meth|
          define_method(meth){@_response.send(meth)}
        end
        [:status, :body, :headers, :mime_type, :content_type, :attachment].each do |meth|
          define_method(meth){|*v, &block| @_response.send(meth, *v, &block)}
        end
      end
    end

    register_plugin(:sinatra_helpers, SinatraHelpers)
  end
end
