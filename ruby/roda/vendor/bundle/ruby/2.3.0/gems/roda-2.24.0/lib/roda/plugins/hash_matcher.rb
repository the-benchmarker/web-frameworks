# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The hash_matcher plugin adds the hash_matcher class method, which
    # allows for easily defining hash matchers:
    #
    #   class App < Roda
    #     hash_matcher(:foo) do |v|
    #       self['foo'] == v
    #     end
    #
    #     route do
    #       r.on :foo=>'bar' do
    #         # matches when param foo has value bar
    #       end
    #     end
    #   end
    module HashMatcher
      module ClassMethods
        # Create a match_#{key} method in the request class using the given
        # block, so that using a hash key in a request match method will
        # call the block.  The block should return nil or false to not
        # match, and anything else to match.  See the HashMatcher module
        # documentation for an example.
        def hash_matcher(key, &block)
          self::RodaRequest.send(:define_method, :"match_#{key}", &block)
        end
      end
    end

    register_plugin(:hash_matcher, HashMatcher)
  end
end
