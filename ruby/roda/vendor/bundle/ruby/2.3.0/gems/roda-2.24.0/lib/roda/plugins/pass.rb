# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The pass plugin adds a request +pass+ method to skip the current match
    # block as if it did not match.
    #
    #   plugin :pass
    #
    #   route do |r|
    #     r.on "foo", :bar do |bar|
    #       r.pass if bar == 'baz'
    #       "/foo/#{bar} (not baz)"
    #     end
    #
    #     r.on "foo/baz" do
    #       "/foo/baz"
    #     end
    #   end
    module Pass
      module RequestMethods
        # Skip the current match block as if it did not match.
        def pass
          throw :pass
        end

        private

        # Handle passing inside the match block.
        def always
          catch(:pass){super}
        end

        # Handle passing inside the match block.
        def if_match(_)
          rp = @remaining_path
          ret = catch(:pass){super}
          @remaining_path = rp
          ret
        end
      end
    end

    register_plugin(:pass, Pass)
  end
end
