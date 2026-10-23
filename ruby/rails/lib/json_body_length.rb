class JsonBodyLength
  def initialize(app)
    @app = app
  end

  def call(env)
    if env['CONTENT_LENGTH'].nil? && env['rack.input'] && env['CONTENT_TYPE']
      request = ActionDispatch::Request.new(env)

      if request.content_mime_type&.json?
        # Some Rack servers remove Transfer-Encoding after decoding without
        # supplying Content-Length. Rails needs a length or Transfer-Encoding
        # to recognize that there is a JSON body to parse.
        body = env['rack.input'].read.to_s
        request.set_header('RAW_POST_DATA', body)
        request.set_header('CONTENT_LENGTH', body.bytesize.to_s)
      end
    end

    @app.call(env)
  end
end
