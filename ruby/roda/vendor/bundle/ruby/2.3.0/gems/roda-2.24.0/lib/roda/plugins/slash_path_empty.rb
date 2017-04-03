# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The slash_path_empty plugin considers "/" as an empty path,
    # in addition to the default of "" being considered an empty
    # path.  This makes it so +r.is+ without an argument will match
    # a path of "/", and +r.is+ and verb methods such as +r.get+ and
    # +r.post+ will match if the path is "/" after the arguments
    # are processed.  This can make it easier to handle applications
    # where a trailing "/" in the path should be ignored.
    module SlashPathEmpty
      SLASH = "/".freeze

      module RequestMethods
        private

        # Consider the path empty if it is "/".
        def empty_path?
          super || remaining_path == SLASH
        end
      end
    end

    register_plugin(:slash_path_empty, SlashPathEmpty)
  end
end
