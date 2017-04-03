# frozen-string-literal: true

require 'json'

class Roda
  module RodaPlugins
    # The json plugin allows match blocks to return
    # arrays or hashes, and have those arrays or hashes be
    # converted to json which is used as the response body.
    # It also sets the response content type to application/json.
    # So you can take code like:
    #
    #   r.root do
    #     response['Content-Type'] = 'application/json'
    #     [1, 2, 3].to_json
    #   end
    #   r.is "foo" do
    #     response['Content-Type'] = 'application/json'
    #     {'a'=>'b'}.to_json
    #   end
    #
    # and DRY it up:
    #
    #   plugin :json
    #   r.root do
    #     [1, 2, 3]
    #   end
    #   r.is "foo" do
    #     {'a'=>'b'}
    #   end
    #
    # By default, only arrays and hashes are handled, but you
    # can specifically set the allowed classes to json by adding
    # using the :classes option when loading the plugin:
    #
    #   plugin :json, :classes=>[Array, Hash, Sequel::Model]
    #
    # By default objects are serialized with +to_json+, but you
    # can pass in a custom serializer, which can be any object
    # that responds to +call(object)+.
    #
    #   plugin :json, :serializer=>proc{|o| o.to_json(root: true)}
    #
    # If you need the request information during serialization, such
    # as HTTP headers or query parameters, you can pass in the
    # +:include_request+ option, which will pass in the request
    # object as the second argument when calling the serializer.
    #
    #   plugin :json, :include_request=>true, :serializer=>proc{|o, request| ...}
    #
    # The default content-type is 'application/json', but you can change that
    # using the +:content_type+ option:
    #
    #   plugin :json, :content_type=>'application/xml'
    module Json
      OPTS = {}.freeze
      DEFAULT_SERIALIZER = lambda{|o| o.to_json}
      DEFAULT_CONTENT_TYPE = 'application/json'.freeze

      # Set the classes to automatically convert to JSON, and the serializer to use.
      def self.configure(app, opts=OPTS)
        classes = opts[:classes] || [Array, Hash]
        app.opts[:json_result_classes] ||= []
        app.opts[:json_result_classes] += classes
        app.opts[:json_result_classes].uniq!
        app.opts[:json_result_classes].freeze

        app.opts[:json_result_serializer] = opts[:serializer] || app.opts[:json_result_serializer] || DEFAULT_SERIALIZER

        app.opts[:json_result_include_request] = opts[:include_request] || app.opts[:json_result_include_request]

        app.opts[:json_result_content_type] = opts[:content_type] || DEFAULT_CONTENT_TYPE
      end

      module ClassMethods
        # The classes that should be automatically converted to json
        def json_result_classes
          opts[:json_result_classes]
        end
      end

      module RequestMethods
        CONTENT_TYPE = 'Content-Type'.freeze

        private

        # If the result is an instance of one of the json_result_classes,
        # convert the result to json and return it as the body, using the
        # application/json content-type.
        def block_result_body(result)
          case result
          when *roda_class.json_result_classes
            response[CONTENT_TYPE] ||= roda_class.opts[:json_result_content_type]
            convert_to_json(result)
          else
            super
          end
        end

        # Convert the given object to JSON.  Uses to_json by default,
        # but can use a custom serializer passed to the plugin.
        def convert_to_json(obj)
          args = [obj]
          args << self if roda_class.opts[:json_result_include_request]
          roda_class.opts[:json_result_serializer].call(*args)
        end
      end
    end

    register_plugin(:json, Json)
  end
end
