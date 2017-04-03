# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The indifferent_params plugin adds a +params+ instance
    # method which offers indifferent access to the request
    # params, allowing you to use symbols to lookup values in
    # a hash where the keys are strings.  Example:
    #
    #   plugin :indifferent_params
    #
    #   route do |r|
    #     params[:foo]
    #   end
    #
    # The exact behavior depends on the version of Rack in use.
    # If you are using Rack 2, this plugin uses rack's API
    # to set the query parser for the request to use indifferent
    # access. Rack 1 doesn't support indifferent access to
    # params, so if you are using Rack 1, this plugin will make
    # a deep copy of the request params hash, where each level
    # uses indifferent access.  On Rack 1, The params hash is
    # initialized lazily, so you only pay the penalty of
    # copying the request params if you call the +params+ method.
    #
    # Note that there is a rack-indifferent gem that
    # monkey patches rack to always use indifferent params. If
    # you are using Rack 1, it is recommended to use
    # rack-indifferent instead of this plugin, as it is faster
    # and has some other minor advantages, though
    # it affects all rack applications instead of just the Roda app that
    # you load the plugin into.
    module IndifferentParams
      INDIFFERENT_PROC = lambda{|h,k| h[k.to_s] if k.is_a?(Symbol)}

      if Rack.release > '2'
        # :nocov:
        class QueryParser < Rack::QueryParser
          # Work around for invalid optimization in rack
          def parse_nested_query(qs, d=nil)
            return make_params.to_params_hash if qs.nil? || qs.empty?
            super
          end
          
          class Params < Rack::QueryParser::Params
            def initialize(limit = Rack::Utils.key_space_limit)
              @limit  = limit
              @size   = 0
              @params = Hash.new(&INDIFFERENT_PROC)
            end
          end

        end

        module RequestMethods
          QUERY_PARSER = Rack::Utils.default_query_parser = QueryParser.new(QueryParser::Params, 65536, 100)

          def query_parser
            QUERY_PARSER
          end
        end

        module InstanceMethods
          def params
            @_request.params
          end
        end
        # :nocov:
      else
        module InstanceMethods
          # A copy of the request params that will automatically
          # convert symbols to strings.
          def params
            @_params ||= indifferent_params(@_request.params)
          end

          private

          # Recursively process the request params and convert
          # hashes to support indifferent access, leaving
          # other values alone.
          def indifferent_params(params)
            case params 
            when Hash
              hash = Hash.new(&INDIFFERENT_PROC)
              params.each{|k, v| hash[k] = indifferent_params(v)}
              hash
            when Array
              params.map{|x| indifferent_params(x)}
            else
              params
            end
          end
        end
      end  
    end

    register_plugin(:indifferent_params, IndifferentParams)
  end
end
