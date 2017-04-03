# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The default_headers plugin accepts a hash of headers,
    # and overrides the default_headers method in the
    # response class to be a copy of the headers.
    #
    # Note that when using this module, you should not
    # attempt to mutate of the values set in the default
    # headers hash.
    #
    # Example:
    #
    #   plugin :default_headers, 'Content-Type'=>'text/csv'
    #
    # You can modify the default headers later by loading the
    # plugin again:
    #
    #   plugin :default_headers, 'Foo'=>'bar'
    #   plugin :default_headers, 'Bar'=>'baz'
    module DefaultHeaders
      # Merge the given headers into the existing default headers, if any.
      def self.configure(app, headers={})
        app.opts[:default_headers] = (app.default_headers || app::RodaResponse::DEFAULT_HEADERS).merge(headers).freeze
      end 

      module ClassMethods
        # The default response headers to use for the current class.
        def default_headers
          opts[:default_headers]
        end
      end

      module ResponseMethods
        # Get the default headers from the related roda class.
        def default_headers
          roda_class.default_headers
        end
      end
    end

    register_plugin(:default_headers, DefaultHeaders)
  end
end
