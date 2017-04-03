# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The delete_empty_headers plugin deletes any headers whose
    # value is set to the empty string.  Because of how default headers are
    # set in Roda, if you have a default header but don't want
    # to set it for a specific request, you need to use this plugin
    # and set the header value to the empty string, and Roda will automatically
    # delete the header.
    module DeleteEmptyHeaders
      module ResponseMethods
        # Delete any empty headers when calling finish
        def finish
          delelete_empty_headers(super)
        end

        # Delete any empty headers when calling finish_with_body
        def finish_with_body(_)
          delelete_empty_headers(super)
        end

        private

        # Delete any empty headers from response
        def delelete_empty_headers(res)
          res[1].delete_if{|_, v| v.is_a?(String) && v.empty?}
          res
        end
      end
    end

    register_plugin(:delete_empty_headers, DeleteEmptyHeaders)
  end
end
