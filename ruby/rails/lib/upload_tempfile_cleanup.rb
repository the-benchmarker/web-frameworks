require 'rack/multipart'
require 'rack/tempfile_reaper'
require 'stringio'

class UploadTempfileCleanup < Rack::TempfileReaper
  MAX_BODY_BYTES = 65_536

  def call(env)
    if env['REQUEST_METHOD'] == 'POST'
      return too_large if env['CONTENT_LENGTH'].to_i > MAX_BODY_BYTES

      if env['rack.input']
        body = env['rack.input'].read(MAX_BODY_BYTES + 1).to_s
        return too_large if body.bytesize > MAX_BODY_BYTES

        env['CONTENT_LENGTH'] = body.bytesize.to_s
        env['rack.input'] = StringIO.new(body)
      end
    end

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

  private

  def too_large
    [413, { 'content-length' => '0', 'cache-control' => 'no-store' }, []]
  end
end
