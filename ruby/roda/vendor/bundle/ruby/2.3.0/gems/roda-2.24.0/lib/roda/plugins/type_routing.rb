# frozen_string_literal: true

#
class Roda
  module RodaPlugins
    # Allows to respond to specific request data types. User agents can request
    # specific data types by either supplying an appropriate +Accept+ header
    # or by appending it as file extension to the path.
    #
    # Example:
    #
    #   plugin :type_routing
    #
    #   route do |r|
    #     r.get 'a' do
    #       r.html{ "<h1>This is the HTML response</h1>" }
    #       r.json{ '{"json": "ok"}' }
    #       r.xml{ "<root>This is the XML response</root>" }
    #       "Unsupported data type"
    #     end
    #   end
    #
    # This application will handle the following paths:
    # /a.html :: HTML response
    # /a.json :: JSON response
    # /a.xml :: XML response
    # /a :: HTML, JSON, or XML response, depending on the Accept header
    #
    # The response +Content-Type+ header will be set to a suitable value when
    # the block is matched.
    #
    # Note that if no match is found, code will continue to execute, which can
    # result in unexpected behaviour.  This should only happen if you do not
    # handle all supported/configured types.  If you want to simplify handling,
    # you can just place the html handling after the other types, without using
    # a separate block:
    #
    #   route do |r|
    #     r.get 'a' do
    #       r.json{ '{"json": "ok"}' }
    #       r.xml{ "<root>This is the XML response</root>" }
    #
    #       "<h1>This is the HTML response</h1>"
    #     end
    #   end
    #
    # This works correctly because Roda assumes the html type by default.
    #
    # To match custom extensions, use the :types option:
    #
    #   plugin :type_routing, :types => {
    #     :yaml => 'application/x-yaml',
    #     :js => 'application/javascript; charset=utf-8',
    #   }
    #
    #   route do |r|
    #     r.get 'a' do
    #       r.yaml{ YAML.dump "YAML data" }
    #       r.js{ "JavaScript code" }
    #       # or:
    #       r.on_type(:js){ "JavaScript code" }
    #       "Unsupported data type"
    #     end
    #   end
    #
    # = Plugin options
    #
    # The following plugin options are supported:
    #
    # :default_type :: The default data type to assume if the client did not
    #                  provide one. Defaults to +:html+.
    # :exclude :: Exclude one or more types from the default set (default set
    #             is :html, :xml, :json).
    # :types :: Mapping from a data type to its MIME-Type. Used both to match
    #           incoming requests and to provide +Content-Type+ values.  If the
    #           value is +nil+, no +Content-Type+ will be set.  The type may
    #           contain media type parameters, which will be sent to the client
    #           but ignored for request matching.
    # :use_extension :: Whether to take the path extension into account.
    #                   Default is +true+.
    # :use_header :: Whether to take the +Accept+ header into account.
    #                Default is +true+.
    module TypeRouting
      ACCEPT_HEADER = 'HTTP_ACCEPT'.freeze
      CONTENT_TYPE_HEADER = 'Content-Type'.freeze

      CONFIGURATION = {
        :mimes => {
          'text/json' => :json,
          'application/json' => :json,
          'text/xml' => :xml,
          'application/xml' => :xml,
          'text/html' => :html,
        }.freeze,
        :types => {
          :json => 'application/json'.freeze,
          :xml => 'application/xml'.freeze,
          :html => 'text/html'.freeze,
        }.freeze,
        :use_extension => true,
        :use_header => true,
        :default_type => :html
      }.freeze

      def self.configure(app, opts = {})
        config = (app.opts[:type_routing] || CONFIGURATION).dup
        [:use_extension, :use_header, :default_type].each do |key|
          config[key] = opts[key] if opts.has_key?(key)
        end

        types = config[:types] = config[:types].dup
        mimes = config[:mimes] = config[:mimes].dup

        Array(opts[:exclude]).each do |type|
          types.delete(type)
          mimes.reject!{|_, v| v == type}
        end

        if mapping = opts[:types]
          types.merge!(mapping)

          mapping.each do |k, v|
            if v
              mimes[v.split(';', 2).first] = k
            end
          end
        end

        types.freeze
        mimes.freeze

        type_keys = config[:types].keys
        config[:extension_regexp] = /(.+)\.(#{Regexp.union(type_keys.map(&:to_s))})\z/

        type_keys.each do |type|
          app::RodaRequest.send(:define_method, type) do |&block|
            on_type(type, &block)
          end
        end

        app.opts[:type_routing] = config.freeze
      end

      module RequestMethods
        # Yields if the given +type+ matches the requested data type and halts
        # the request afterwards, returning the result of the block.
        def on_type(type, &block)
          return unless type == requested_type
          response[CONTENT_TYPE_HEADER] ||= @scope.opts[:type_routing][:types][type]
          always(&block)
        end

        # Returns the data type the client requests.
        def requested_type
          return @requested_type if defined?(@requested_type)

          opts = @scope.opts[:type_routing]
          @requested_type = accept_response_type if opts[:use_header]
          @requested_type ||= opts[:default_type]
        end

        # Append the type routing extension back to the path if it was
        # removed before routing.
        def real_remaining_path
          if defined?(@type_routing_extension)
            "#{super}.#{@type_routing_extension}"
          else
            super
          end
        end

        private

        # Removes a trailing file extension from the path, and sets
        # the requested type if so.
        def _remaining_path(env)
          opts = scope.opts[:type_routing]
          path = super

          if opts[:use_extension]
            if m = path.match(opts[:extension_regexp])
              @type_routing_extension =  @requested_type = m[2].to_sym
              path = m[1]
            end
          end

          path
        end

        # The response type indicated by the Accept request header.
        def accept_response_type
          mimes = @scope.opts[:type_routing][:mimes]

          @env[ACCEPT_HEADER].to_s.split(/\s*,\s*/).map do |part|
            mime, _= part.split(/\s*;\s*/, 2)
            if sym = mimes[mime]
              return sym
            end
          end

          nil
        end
      end
    end

    register_plugin(:type_routing, TypeRouting)
  end
end
