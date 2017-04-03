# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The params_capturing plugin makes symbol matchers
    # update the request params with the value of the captured segments,
    # using the matcher as the key:
    #
    #   plugin :params_capturing
    #
    #   route do |r|
    #     # GET /foo/123/abc/67
    #     r.on("foo", :bar, :baz, :quux) do
    #       r[:bar] #=> '123'
    #       r[:baz] #=> 'abc'
    #       r[:quux] #=> '67'
    #     end
    #   end
    #
    # Note that this updating of the request params using the matcher as
    # the key is only done if all arguments to the matcher are symbols
    # or strings.
    #
    # All matchers will update the request params by adding all
    # captured segments to the +captures+ key:
    #
    #   r.on(:x, /(\d+)\/(\w+)/, :y) do
    #     r[:x] #=> nil
    #     r[:y] #=> nil
    #     r[:captures] #=> ["foo", "123", "abc", "67"]
    #   end
    #
    # Note that the request params +captures+ entry will be appended to with
    # each nested match:
    #
    #   r.on(:w) do
    #     r.on(:x) do
    #       r.on(:y) do
    #         r.on(:z) do
    #           r[:captures] # => ["foo", "123", "abc", "67"]
    #         end
    #       end
    #     end
    #   end
    #
    # Note that any existing params captures entry will be overwritten
    # by this plugin.  You can use +r.GET+ or +r.POST+ to get the underlying
    # entry, depending on how it was submitted.
    #
    # Also note that the param keys are actually stored in +r.params+ as
    # strings and not symbols (<tt>r[]</tt> converts the argument
    # to a string before looking it up in +r.params+).
    #
    # This plugin will also handle string matchers if placeholders in
    # string matchers are supported.
    #
    # Also note that this plugin will not work correctly if you are using
    # the symbol_matchers plugin with custom symbol matching and are using
    # symbols that capture multiple values or no values.
    module ParamsCapturing
      module RequestMethods
        def initialize(*)
          super
          params['captures'] = []
        end

        private

        if RUBY_VERSION >= '1.9'
          # Regexp to scan for capture names. Uses positive lookbehind
          # so it is only valid on ruby 1.9+, hence the use of eval.
          STRING_PARAM_CAPTURE_REGEXP = eval("/(?<=:)\\w+/")

          # Add the capture names from this string to list of param
          # capture names if param capturing.
          def _match_string(str)
            if (pc = @_params_captures) && placeholder_string_matcher?
              pc.concat(str.scan(STRING_PARAM_CAPTURE_REGEXP))
            end
            super
          end
        else
          # :nocov:

          # Ruby 1.8 doesn't support positive lookbehind, so include the
          # colon in the scan, and strip it out later.
          STRING_PARAM_CAPTURE_RANGE = 1..-1

          def _match_string(str)
            if (pc = @_params_captures) && placeholder_string_matcher?
              pc.concat(str.scan(/:\w+/).map{|s| s[STRING_PARAM_CAPTURE_RANGE]})
            end
            super
          end
          # :nocov:
        end

        # Add the symbol to the list of param capture names if param capturing.
        def _match_symbol(sym)
          if pc = @_params_captures
            pc << sym.to_s
          end
          super
        end

        # If all arguments are strings or symbols, turn on param capturing during
        # the matching, but turn it back off before yielding to the block.  Add
        # any captures to the params based on the param capture names added by
        # the matchers.
        def if_match(args)
          params = self.params

          if args.all?{|x| x.is_a?(String) || x.is_a?(Symbol)}
            pc = @_params_captures = []
          end

          super do |*a|
            if pc
              @_params_captures = nil
              pc.zip(a).each do |k,v|
                params[k] = v
              end
            end
            params['captures'].concat(a) 
            yield(*a)
          end
        end
      end
    end

    register_plugin(:params_capturing, ParamsCapturing)
  end
end
