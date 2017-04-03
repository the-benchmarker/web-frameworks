# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The match_affix plugin allows changing the default prefix and suffix used for
    # match patterns.  Roda's default behavior for a match pattern like <tt>"albums"</tt>
    # is to use the pattern <tt>/\A\/(?:albums)(?=\/|\z)/</tt>.  This prefixes the pattern
    # with +/+ and suffixes it with <tt>(?=\/|\z)</tt>.  With the match_affix plugin, you
    # can change the prefix and suffix to use.  So if you want to be explicit and require
    # a leading +/+ in patterns, you can set the prefix to <tt>""</tt>.  If you want to
    # consume a trailing slash instead of leaving it, you can set the suffix to <tt>(\/|\z)</tt>.
    #
    # You set the prefix and suffix to use by passing arguments when loading the plugin:
    #
    #   plugin :match_affix, ""
    #
    # will load the plugin and use an empty prefix (instead of a slash).
    #
    #   plugin :match_affix, "", /(\/|\z)/
    #
    # will use an empty prefix and change the suffix to consume a trailing slash.
    #
    #  plugin :match_affix, nil, /(?:\/\z|(?=\/|\z))/
    #
    # will not modify the prefix and will change the suffix so that it consumes a trailing slash
    # at the end of the path only.
    module MatchAffix
      PREFIX = "/".freeze
      SUFFIX = "(?=\/|\z)".freeze

      # Set the default prefix and suffix to use in match patterns, if a non-nil value
      # is given.
      def self.configure(app, prefix, suffix=nil)
        app.opts[:match_prefix] = prefix if prefix
        app.opts[:match_suffix] = suffix if suffix
      end
      
      module RequestClassMethods
        private

        # Use the match prefix and suffix provided when loading the plugin, or fallback
        # to Roda's default prefix/suffix if one was not provided.
        def consume_pattern(pattern)
          /\A#{roda_class.opts[:match_prefix] || PREFIX}(?:#{pattern})#{roda_class.opts[:match_suffix] || SUFFIX}/
        end
      end

      module RequestMethods
        private

        # Use regexps for all string matches, so that the prefix and suffix matches work.
        def _match_string(str)
          consume(self.class.cached_matcher(str){Regexp.escape(str).gsub(/:(\w+)/){|m| _match_symbol_regexp($1)}})
        end
      end
    end

    register_plugin(:match_affix, MatchAffix)
  end
end
