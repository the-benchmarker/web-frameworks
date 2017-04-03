# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The param_matchers plugin adds hash matchers that operate
    # on the request's params.
    #
    # It adds a :param matcher for matching on any param with the
    # same name, yielding the value of the param:
    #
    #   r.on :param => 'foo' do |value|
    #     # Matches '?foo=bar', '?foo='
    #     # Doesn't match '?bar=foo'
    #   end
    #
    # It adds a :param! matcher for matching on any non-empty param
    # with the same name, yielding the value of the param:
    #
    #   r.on(:param! => 'foo') do |value|
    #     # Matches '?foo=bar'
    #     # Doesn't match '?foo=', '?bar=foo'
    #   end
    #
    # It also adds :params and :params! matchers, for matching multiple
    # params at the same time:
    #
    #   r.on :params => ['foo', 'baz'] do |value|
    #     # Matches '?foo=bar&baz=quuz', '?foo=&baz='
    #     # Doesn't match '?foo=bar', '?baz='
    #   end
    #
    #   r.on :params! => ['foo', 'baz'] do |value|
    #     # Matches '?foo=bar&baz=quuz'
    #     # Doesn't match '?foo=bar', '?baz=', '?foo=&baz=', '?foo=bar&baz='
    #   end
    #
    module ParamMatchers
      module RequestMethods
        # Match the given parameter if present, even if the parameter is empty.
        # Adds match to the captures.
        def match_param(key)
          if v = self[key]
            @captures << v
          end
        end

        # Match the given parameter if present and not empty.
        # Adds match to the captures.
        def match_param!(key)
          if (v = self[key]) && !v.empty?
            @captures << v
          end
        end

        # Match all given parameters if present, even if any/all parameters is empty.
        # Adds all matches to the captures.
        def match_params(keys)
          keys.each do |key|
            return false unless match_param(key)
          end
        end

        # Match all given parameters if present and not empty.
        # Adds all matches to the captures.
        def match_params!(keys)
          keys.each do |key|
            return false unless match_param!(key)
          end
        end
      end
    end

    register_plugin(:param_matchers, ParamMatchers)
  end
end
