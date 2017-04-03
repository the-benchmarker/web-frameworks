# frozen-string-literal: true

raise LoadError, "disallow_file_uploads plugin not supported on Rack <1.6" if Rack.release < '1.6'

#
class Roda
  module RodaPlugins
    # The disallow_file_uploads plugin raises a Roda::RodaPlugins::DisallowFileUploads::Error
    # if there is an attempt to upload a file.  This plugin is useful for applications where
    # multipart file uploads are not expected and you want to remove the ability for rack
    # to create temporary files.  Example:
    #
    #   plugin :disallow_file_uploads
    #
    # This plugin is only supported on Rack 1.6+.  This plugin does not technically
    # block users from uploading files, it only blocks the parsing of request bodies containing
    # multipart file uploads.  So if you do not call +r.POST+ (or something that calls it such as
    # +r.params+), then Roda will not attempt to parse the request body, and an exception will not
    # be raised.
    module DisallowFileUploads
      # Exception class used when a multipart file upload is attempted.
      class Error < RodaError; end

      NO_TEMPFILE = lambda{|_,_| raise Error, "Support for uploading files has been disabled"}

      module RequestMethods
        # HTML escape the input and return the escaped version.
        def initialize(_, env)
          env['rack.multipart.tempfile_factory'] = NO_TEMPFILE
          super
        end
      end
    end

    register_plugin(:disallow_file_uploads, DisallowFileUploads)
  end
end

