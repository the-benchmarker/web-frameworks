require 'stringio'

# Bound input before Rails parses parameters, including chunked requests whose
# size is unknown to the controller. The limit includes multipart boundaries.
class RequestBodyLimit
  MAX_BYTES = 64 * 1024

  def initialize(app)
    @app = app
  end

  def call(env)
    # Limit every POST so Rails' format suffixes and normalized paths cannot
    # bypass the check (for example, /upload.json or /upload/).
    return @app.call(env) unless env['REQUEST_METHOD'] == 'POST'

    return too_large if env['CONTENT_LENGTH'].to_i > MAX_BYTES

    # Rack permits requests without an input stream when there is no body.
    return @app.call(env) unless env['rack.input']

    body = env['rack.input'].read(MAX_BYTES + 1).to_s
    return too_large if body.bytesize > MAX_BYTES

    # Expose the buffered length even when a server omits CONTENT_LENGTH.
    env['CONTENT_LENGTH'] = body.bytesize.to_s
    env['rack.input'] = StringIO.new(body)
    @app.call(env)
  end

  private

  def too_large
    [413, { 'content-length' => '0', 'cache-control' => 'no-store' }, []]
  end
end
