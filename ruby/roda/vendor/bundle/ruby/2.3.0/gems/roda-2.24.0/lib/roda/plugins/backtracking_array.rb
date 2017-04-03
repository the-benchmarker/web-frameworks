# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The backtracking_array plugin changes the handling of array
    # matchers such that if one of the array entries matches, but
    # a later match argument fails, it will backtrack and try the
    # next entry in the array.  For example, the following match
    # block does not match +/a/b+ by default:
    #
    #   r.is ['a', 'a/b'] do |path|
    #     # ...
    #   end
    #
    # This is because the <tt>'a'</tt> entry in the array matches, which
    # makes the array match.  However, the next matcher is the
    # terminal matcher (since +r.is+ was used), and since the
    # path is not terminal as it still contains +/b+ after
    # matching <tt>'a'</tt>.
    #
    # With the backtracking_array plugin, when the terminal matcher
    # fails, matching will go on to the next entry in the array,
    # <tt>'a/b'</tt>, which will also match.  Since <tt>'a/b'</tt>
    # matches the path fully, the terminal matcher also matches,
    # and the match block yields.
    module BacktrackingArray
      module RequestMethods
        PATH_INFO = "PATH_INFO".freeze
        SCRIPT_NAME = "SCRIPT_NAME".freeze

        private

        # When matching for a single array, after a successful
        # array element match, attempt to match all remaining
        # elements.  If the remaining elements could not be
        # matched, reset the state and continue to the next
        # entry in the array.
        def _match_array(arg, rest=nil)
          return super unless rest

          path = @remaining_path
          captures = @captures
          caps = captures.dup
          arg.each do |v|
            if match(v, rest)
              if v.is_a?(String)
                captures.push(v)
              end

              if match_all(rest)
                return true
              end

              # Matching all remaining elements failed, reset state
              captures.replace(caps)
              @remaining_path = path
            end
          end
          false
        end

        # If any of the args are an array, handle backtracking such
        # that if a later matcher fails, we roll back to the current
        # matcher and proceed to the next entry in the array.
        def match_all(args)
          args = args.dup
          until args.empty?
            arg = args.shift
            if match(arg, args)
              return true if arg.is_a?(Array)
            else
              return
            end
          end
          true
        end

        # When matching an array, include the remaining arguments,
        # otherwise, just match the single argument.
        def match(v, rest = nil)
          if v.is_a?(Array)
            _match_array(v, rest)
          else
            super(v)
          end
        end
      end
    end

    register_plugin(:backtracking_array, BacktrackingArray)
  end
end
