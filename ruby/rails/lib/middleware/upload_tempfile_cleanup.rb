require 'rack/multipart'
require 'rack/tempfile_reaper'
require 'rack/media_type'

class UploadTempfileCleanup < Rack::TempfileReaper
  def call(env)
    return @app.call(env) unless Rack::MediaType.type(env['CONTENT_TYPE']) == 'multipart/form-data'

    tempfiles = env['rack.tempfiles'] ||= []
    factory = env['rack.multipart.tempfile_factory'] || Rack::Multipart::Parser::TEMPFILE_FACTORY

    # Rack normally registers files only after parsing succeeds. Register each
    # file immediately so its reaper also cleans up truncated or invalid input.
    env['rack.multipart.tempfile_factory'] = lambda do |filename, content_type|
      tempfile = factory.call(filename, content_type)
      tempfiles << tempfile
      tempfile
    end

    super
  end
end
