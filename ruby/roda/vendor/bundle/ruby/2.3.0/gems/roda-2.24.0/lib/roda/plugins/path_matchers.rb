# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The path_matchers plugin adds hash matchers that operate
    # on the request's path.
    #
    # It adds a :prefix matcher for matching on the path's prefix,
    # yielding the rest of the matched segment:
    #
    #   r.on :prefix=>'foo' do |suffix|
    #     # Matches '/foo-bar', yielding '-bar'
    #     # Does not match bar-foo
    #   end
    #
    # It adds a :suffix matcher for matching on the path's suffix,
    # yielding the part of the segment before the suffix:
    #
    #   r.on :suffix=>'bar' do |prefix|
    #     # Matches '/foo-bar', yielding 'foo-'
    #     # Does not match bar-foo
    #   end
    #
    # It adds an :extension matcher for matching on the given file extension,
    # yielding the part of the segment before the extension:
    #
    #   r.on :extension=>'bar' do |reset|
    #     # Matches '/foo.bar', yielding 'foo'
    #     # Does not match bar.foo
    #   end
    module PathMatchers
      module RequestMethods
        # Match when the current segment ends with the given extension.
        # request path end with the extension.
        def match_extension(ext)
          match_suffix(".#{ext}")
        end

        # Match when the current path segment starts with the given prefix.
        def match_prefix(prefix)
          consume(self.class.cached_matcher([:prefix, prefix]){/#{prefix}([^\\\/]+)/})
        end

        # Match when the current path segment ends with the given suffix.
        def match_suffix(suffix)
          consume(self.class.cached_matcher([:suffix, suffix]){/([^\\\/]+)#{suffix}/})
        end
      end
    end

    register_plugin(:path_matchers, PathMatchers)
  end
end
