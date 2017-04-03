# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The drop_body plugin automatically drops the body and
    # Content-Type/Content-Length headers from the response if
    # the response status indicates that the response should
    # not include a body (response statuses 100, 101, 102, 204, 205,
    # and 304).
    module DropBody
      module ResponseMethods
        DROP_BODY_STATUSES = [100, 101, 102, 204, 205, 304].freeze
        EMPTY_BODY = [].freeze
        CONTENT_LENGTH = "Content-Length".freeze
        CONTENT_TYPE = "Content-Type".freeze

        # If the response status indicates a body should not be
        # returned, use an empty body and remove the Content-Length
        # and Content-Type headers.
        def finish
          r = super
          if DROP_BODY_STATUSES.include?(r[0])
            r[2] = EMPTY_BODY
            h = r[1]
            h.delete(CONTENT_LENGTH)
            h.delete(CONTENT_TYPE)
          end
          r
        end
      end
    end

    register_plugin(:drop_body, DropBody)
  end
end
