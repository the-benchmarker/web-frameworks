# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The chunked plugin allows you to stream responses to clients using
    # Transfer-Encoding: chunked.  This can significantly improve performance
    # of page rendering on the client, as it flushes the headers and top part
    # of the layout template (generally containing references to the stylesheet
    # and javascript assets) before rendering the content template.
    #
    # This allows the client to fetch the assets while the template is still
    # being rendered.  Additionally, this plugin makes it easy to defer
    # executing code required to render the content template until after
    # the top part of the layout has been flushed, so the client can fetch the
    # assets while the application is still doing the necessary processing in
    # order to render the content template, such as retrieving values from a
    # database.
    #
    # There are a couple disadvantages of streaming using chunked encoding.
    # First is that the layout must be rendered before the content, so any state 
    # changes made in your content template will not affect the layout template.
    # Second, error handling is reduced, since if an error occurs while
    # rendering a template, a successful response code has already been sent.
    #
    # To use chunked encoding for a response, just call the chunked method
    # instead of view:
    #
    #   r.root do
    #     chunked(:index)
    #   end
    #
    # If you want to execute code after flushing the top part of the layout
    # template, but before rendering the content template, pass a block to
    # chunked:
    #
    #   r.root do
    #     chunked(:index) do
    #       # expensive calculation here
    #     end
    #   end
    #
    # You can also call delay manually with a block, and the execution of the
    # block will be delayed until rendering the content template.  This is
    # useful if you want to delay execution for all routes under a branch:
    #
    #   r.on 'albums', :d do |album_id|
    #     delay do
    #       @album = Album[album_id]
    #     end
    #     r.get 'info' do
    #       chunked(:info)
    #     end
    #     r.get 'tracks' do
    #       chunked(:tracks)
    #     end
    #   end
    #
    #
    # If you want to chunk all responses, pass the :chunk_by_default option
    # when loading the plugin:
    #
    #   plugin :chunked, :chunk_by_default => true
    #
    # then you can just use the normal view method:
    #
    #   r.root do
    #     view(:index)
    #   end
    #
    # and it will chunk the response.  Note that you still need to call
    # chunked if you want to pass a block of code to be executed after flushing
    # the layout and before rendering the content template. Also, before you
    # enable chunking by default, you need to make sure that none of your
    # content templates make state changes that affect the layout template.
    # Additionally, make sure nowhere in your app are you doing any processing
    # after the call to view.
    #
    # If you use :chunk_by_default, but want to turn off chunking for a view,
    # call no_chunk!:
    #
    #   r.root do
    #     no_chunk!
    #     view(:index)
    #   end
    #
    # Inside your layout or content templates, you can call the flush method
    # to flush the current result of the template to the user, useful for
    # streaming large datasets.
    #
    #   <% (1..100).each do |i| %>
    #     <%= i %>
    #     <% sleep 0.1 %>
    #     <% flush %>
    #   <% end %>
    #
    # Note that you should not call flush from inside subtemplates of the
    # content or layout templates, unless you are also calling flush directly
    # before rendering the subtemplate, and also directly injecting the
    # subtemplate into the current template without modification.  So if you
    # are using the above template code in a subtemplate, in your content
    # template you should do:
    #
    #   <% flush %><%= render(:subtemplate) %>
    #
    # If you want to use chunked encoding when rendering a template, but don't
    # want to use a layout, pass the :layout=>false option to chunked.
    #
    #   r.root do
    #     chunked(:index, :layout=>false)
    #   end
    #
    # In order to handle errors in chunked responses, you can override the
    # handle_chunk_error method:
    #
    #   def handle_chunk_error(e)
    #     env['rack.logger'].error(e)
    #   end
    #
    # It is possible to set @_out_buf to an error notification and call
    # flush to output the message to the client inside handle_chunk_error.
    #
    # In order for chunking to work, you must make sure that no proxies between
    # the application and the client buffer responses.  Also, this
    # plugin only works for HTTP/1.1 requests since Transfer-Encoding: chunked
    # is not supported in HTTP/1.0.  If an HTTP/1.0 request is submitted, this
    # plugin will automatically fallback to the normal template rendering.
    # Note that some proxies including nginx default to HTTP/1.0 even if the
    # client supports HTTP/1.1.  For nginx, set the proxy_http_version to 1.1.
    #
    # If you are using nginx and have it set to buffer proxy responses by
    # default, you can turn this off on a per response basis using the
    # X-Accel-Buffering header.  To set this header or similar headers for
    # all chunked responses, pass a :headers option when loading the plugin:
    #
    #   plugin :chunked, :headers=>{'X-Accel-Buffering'=>'no'}
    #
    # The chunked plugin requires the render plugin, and only works for
    # template engines that store their template output variable in
    # @_out_buf.  Also, it only works if the content template is directly
    # injected into the layout template without modification.
    #
    # If using the chunked plugin with the flash plugin, make sure you
    # call the flash method early in your route block.  If the flash
    # method is not called until template rendering, the flash may not be
    # rotated.
    module Chunked
      HTTP_VERSION = 'HTTP_VERSION'.freeze
      HTTP11 = "HTTP/1.1".freeze
      TRANSFER_ENCODING = 'Transfer-Encoding'.freeze
      CHUNKED = 'chunked'.freeze
      OPTS = {}.freeze

      # Depend on the render plugin
      def self.load_dependencies(app, opts=OPTS)
        app.plugin :render
      end

      # Set plugin specific options.  Options:
      # :chunk_by_default :: chunk all calls to view by default
      # :headers :: Set default additional headers to use when calling view
      def self.configure(app, opts=OPTS)
        app.opts[:chunk_by_default] = opts[:chunk_by_default]
        if opts[:headers]
          app.opts[:chunk_headers] = (app.opts[:chunk_headers] || {}).merge(opts[:headers]).freeze
        end
      end

      # Rack response body instance for chunked responses
      class Body
        CHUNK_SIZE = "%x\r\n".freeze
        CRLF = "\r\n".freeze
        FINISH = "0\r\n\r\n".freeze

        # Save the scope of the current request handling.
        def initialize(scope)
          @scope = scope
        end

        # For each response chunk yielded by the scope,
        # yield it it to the caller in chunked format, starting
        # with the size of the request in ASCII hex format, then
        # the chunk.  After all chunks have been yielded, yield
        # a 0 sized chunk to finish the response.
        def each
          @scope.each_chunk do |chunk|
            next if !chunk || chunk.empty?
            yield(CHUNK_SIZE % chunk.bytesize)
            yield(chunk)
            yield(CRLF)
          end
        ensure
          yield(FINISH)
        end
      end

      module InstanceMethods
        # Disable chunking for the current request.  Mostly useful when
        # chunking is turned on by default.
        def no_chunk!
          @_chunked = false
        end

        # If chunking by default, call chunked if it hasn't yet been
        # called and chunking is not specifically disabled.
        def view(*a)
          if opts[:chunk_by_default] && !defined?(@_chunked)
            chunked(*a)
          else
            super
          end
        end

        # Render a response to the user in chunks.  See Chunked for
        # an overview.  If a block is given, it is passed to #delay.
        def chunked(template, opts=OPTS, &block)
          unless defined?(@_chunked)
            @_chunked = env[HTTP_VERSION] == HTTP11
          end

          if block
            delay(&block)
          end

          unless @_chunked
            # If chunking is disabled, do a normal rendering of the view.
            run_delayed_blocks
            return view(template, opts)
          end

          if template.is_a?(Hash)
            if opts.empty?
              opts = template
            else
              opts = Hash[opts].merge!(template)
            end
          end
          
          # Hack so that the arguments don't need to be passed
          # through the response and body objects.
          @_each_chunk_args = [template, opts]

          res = response
          headers = res.headers
          if chunk_headers = self.opts[:chunk_headers]
            headers.merge!(chunk_headers)
          end
          headers[TRANSFER_ENCODING] = CHUNKED

          throw :halt, res.finish_with_body(Body.new(self))
        end

        # Delay the execution of the block until right before the
        # content template is to be rendered.
        def delay(&block)
          raise RodaError, "must pass a block to Roda#delay" unless block
          (@_delays ||= []) << block
        end

        # Yield each chunk of the template rendering separately.
        def each_chunk
          response.body.each{|s| yield s}

          template, opts = @_each_chunk_args

          # Use a lambda for the flusher, so that a call to flush
          # by a template can result in this method yielding a chunk
          # of the response.
          @_flusher = lambda do
            yield @_out_buf
            @_out_buf = String.new
          end

          if layout_opts  = view_layout_opts(opts)
            @_out_buf = render_template(layout_opts) do
              flush
              run_delayed_blocks
              yield opts[:content] || render_template(template, opts)
              nil
            end
          else
            run_delayed_blocks
            yield view(template, opts)
          end

          flush
        rescue => e
          handle_chunk_error(e)
        end

        # By default, raise the exception.
        def handle_chunk_error(e)
          raise e
        end

        # Call the flusher if one is defined.  If one is not defined, this
        # is a no-op, so flush can be used inside views without breaking
        # things if chunking is not used.
        def flush
          @_flusher.call if @_flusher
        end
        
        private

        # Run all delayed blocks
        def run_delayed_blocks
          return unless @_delays
          @_delays.each(&:call)
        end
      end
    end

    register_plugin(:chunked, Chunked)
  end
end
